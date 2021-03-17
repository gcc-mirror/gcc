
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/identifier.c
 */

#include "root/dsystem.h"
#include "root/root.h"

#include "identifier.h"
#include "mars.h"
#include "id.h"
#include "tokens.h"
#include "utf.h"

Identifier::Identifier(const char *string, size_t length, int value)
{
    //printf("Identifier('%s', %d)\n", string, value);
    this->string = string;
    this->value = value;
    this->len = length;
}

Identifier::Identifier(const char *string)
{
    //printf("Identifier('%s')\n", string);
    this->string = string;
    this->value = TOKidentifier;
    this->len = strlen(string);
}

Identifier *Identifier::create(const char *string)
{
    return new Identifier(string);
}

bool Identifier::equals(RootObject *o)
{
    return this == o || strncmp(string,o->toChars(),len+1) == 0;
}

int Identifier::compare(RootObject *o)
{
    return strncmp(string, o->toChars(), len + 1);
}

const char *Identifier::toChars()
{
    return string;
}

int Identifier::getValue() const
{
    return value;
}

const char *Identifier::toHChars2()
{
    const char *p = NULL;

    if (this == Id::ctor) p = "this";
    else if (this == Id::dtor) p = "~this";
    else if (this == Id::unitTest) p = "unittest";
    else if (this == Id::dollar) p = "$";
    else if (this == Id::withSym) p = "with";
    else if (this == Id::result) p = "result";
    else if (this == Id::returnLabel) p = "return";
    else
    {   p = toChars();
        if (*p == '_')
        {
            if (startswith(p, "_staticCtor"))
                p = "static this";
            else if (startswith(p, "_staticDtor"))
                p = "static ~this";
            else if (startswith(p, "__invariant"))
                p = "invariant";
        }
    }

    return p;
}

void Identifier::print()
{
    fprintf(stderr, "%s",string);
}

int Identifier::dyncast() const
{
    return DYNCAST_IDENTIFIER;
}

StringTable Identifier::stringtable;

Identifier *Identifier::generateId(const char *prefix)
{
    static size_t i;

    return generateId(prefix, ++i);
}

Identifier *Identifier::generateId(const char *prefix, size_t i)
{   OutBuffer buf;

    buf.writestring(prefix);
    buf.printf("%llu", (ulonglong)i);

    char *id = buf.peekChars();
    return idPool(id);
}

/********************************************
 * Create an identifier in the string table.
 */

Identifier *Identifier::idPool(const char *s, size_t len)
{
    StringValue *sv = stringtable.update(s, len);
    Identifier *id = (Identifier *) sv->ptrvalue;
    if (!id)
    {
        id = new Identifier(sv->toDchars(), len, TOKidentifier);
        sv->ptrvalue = (char *)id;
    }
    return id;
}

Identifier *Identifier::idPool(const char *s, size_t len, int value)
{
    StringValue *sv = stringtable.insert(s, len, NULL);
    assert(sv);
    Identifier *id = new Identifier(sv->toDchars(), len, value);
    sv->ptrvalue = (char *)id;
    return id;
}

/**********************************
 * Determine if string is a valid Identifier.
 * Returns:
 *      0       invalid
 */

bool Identifier::isValidIdentifier(const char *p)
{
    size_t len;
    size_t idx;

    if (!p || !*p)
        goto Linvalid;

    if (*p >= '0' && *p <= '9')         // beware of isdigit() on signed chars
        goto Linvalid;

    len = strlen(p);
    idx = 0;
    while (p[idx])
    {
        dchar_t dc;
        const char *q = utf_decodeChar((const utf8_t *)p, len, &idx, &dc);
        if (q)
            goto Linvalid;

        if (!((dc >= 0x80 && isUniAlpha(dc)) || isalnum(dc) || dc == '_'))
            goto Linvalid;
    }
    return true;

Linvalid:
    return false;
}

Identifier *Identifier::lookup(const char *s, size_t len)
{
    StringValue *sv = stringtable.lookup(s, len);
    if (!sv)
        return NULL;
    return (Identifier *)sv->ptrvalue;
}

void Identifier::initTable()
{
    stringtable._init(28000);
}
