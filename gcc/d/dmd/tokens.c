
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/lexer.c
 */

#include "root/dsystem.h"

#include "tokens.h"
#include "root/rmem.h"
#include "root/outbuffer.h"
#include "id.h"
#include "identifier.h"
#include "utf.h"

/************************* Token **********************************************/

Token *Token::freelist = NULL;

const char *Token::tochars[TOKMAX];

Token *Token::alloc()
{
    if (Token::freelist)
    {
        Token *t = freelist;
        freelist = t->next;
        t->next = NULL;
        return t;
    }

    return new Token();
}

void Token::free()
{
    next = freelist;
    freelist = this;
}

const char *Token::toChars() const
{
    static char buffer[3 + 3 * sizeof(floatvalue) + 1];

    const char *p = &buffer[0];
    switch (value)
    {
        case TOKint32v:
            sprintf(&buffer[0],"%d",(d_int32)int64value);
            break;

        case TOKuns32v:
        case TOKcharv:
        case TOKwcharv:
        case TOKdcharv:
            sprintf(&buffer[0],"%uU",(d_uns32)uns64value);
            break;

        case TOKint64v:
            sprintf(&buffer[0],"%lldL",(longlong)int64value);
            break;

        case TOKuns64v:
            sprintf(&buffer[0],"%lluUL",(ulonglong)uns64value);
            break;

        case TOKfloat32v:
            CTFloat::sprint(&buffer[0], 'g', floatvalue);
            strcat(&buffer[0], "f");
            break;

        case TOKfloat64v:
            CTFloat::sprint(&buffer[0], 'g', floatvalue);
            break;

        case TOKfloat80v:
            CTFloat::sprint(&buffer[0], 'g', floatvalue);
            strcat(&buffer[0], "L");
            break;

        case TOKimaginary32v:
            CTFloat::sprint(&buffer[0], 'g', floatvalue);
            strcat(&buffer[0], "fi");
            break;

        case TOKimaginary64v:
            CTFloat::sprint(&buffer[0], 'g', floatvalue);
            strcat(&buffer[0], "i");
            break;

        case TOKimaginary80v:
            CTFloat::sprint(&buffer[0], 'g', floatvalue);
            strcat(&buffer[0], "Li");
            break;

        case TOKstring:
        {
            OutBuffer buf;
            buf.writeByte('"');
            for (size_t i = 0; i < len; )
            {
                unsigned c;
                utf_decodeChar((utf8_t *)ustring, len, &i, &c);
                switch (c)
                {
                    case 0:
                        break;

                    case '"':
                    case '\\':
                        buf.writeByte('\\');
                        /* fall through */
                    default:
                        if (c <= 0x7F)
                        {
                            if (isprint(c))
                                buf.writeByte(c);
                            else
                                buf.printf("\\x%02x", c);
                        }
                        else if (c <= 0xFFFF)
                            buf.printf("\\u%04x", c);
                        else
                            buf.printf("\\U%08x", c);
                        continue;
                }
                break;
            }
            buf.writeByte('"');
            if (postfix)
                buf.writeByte(postfix);
            p = buf.extractChars();
        }
            break;

        case TOKxstring:
        {
            OutBuffer buf;
            buf.writeByte('x');
            buf.writeByte('"');
            for (size_t i = 0; i < len; i++)
            {
                if (i)
                    buf.writeByte(' ');
                buf.printf("%02x", ustring[i]);
            }
            buf.writeByte('"');
            if (postfix)
                buf.writeByte(postfix);
            buf.writeByte(0);
            p = (char *)buf.extractData();
            break;
        }

        case TOKidentifier:
        case TOKenum:
        case TOKstruct:
        case TOKimport:
        case TOKwchar: case TOKdchar:
        case TOKbool: case TOKchar:
        case TOKint8: case TOKuns8:
        case TOKint16: case TOKuns16:
        case TOKint32: case TOKuns32:
        case TOKint64: case TOKuns64:
        case TOKint128: case TOKuns128:
        case TOKfloat32: case TOKfloat64: case TOKfloat80:
        case TOKimaginary32: case TOKimaginary64: case TOKimaginary80:
        case TOKcomplex32: case TOKcomplex64: case TOKcomplex80:
        case TOKvoid:
            p = ident->toChars();
            break;

        default:
            p = toChars(value);
            break;
    }
    return p;
}

const char *Token::toChars(TOK value)
{
    static char buffer[3 + 3 * sizeof(value) + 1];

    const char *p = tochars[value];
    if (!p)
    {
        sprintf(&buffer[0],"TOK%d",value);
        p = &buffer[0];
    }
    return p;
}

/****************************************
 */

struct Keyword
{
    const char *name;
    TOK value;
};

static size_t nkeywords;
static Keyword keywords[] =
{
    {   "this",         TOKthis         },
    {   "super",        TOKsuper        },
    {   "assert",       TOKassert       },
    {   "null",         TOKnull         },
    {   "true",         TOKtrue         },
    {   "false",        TOKfalse        },
    {   "cast",         TOKcast         },
    {   "new",          TOKnew          },
    {   "delete",       TOKdelete       },
    {   "throw",        TOKthrow        },
    {   "module",       TOKmodule       },
    {   "pragma",       TOKpragma       },
    {   "typeof",       TOKtypeof       },
    {   "typeid",       TOKtypeid       },

    {   "template",     TOKtemplate     },

    {   "void",         TOKvoid         },
    {   "byte",         TOKint8         },
    {   "ubyte",        TOKuns8         },
    {   "short",        TOKint16        },
    {   "ushort",       TOKuns16        },
    {   "int",          TOKint32        },
    {   "uint",         TOKuns32        },
    {   "long",         TOKint64        },
    {   "ulong",        TOKuns64        },
    {   "cent",         TOKint128,      },
    {   "ucent",        TOKuns128,      },
    {   "float",        TOKfloat32      },
    {   "double",       TOKfloat64      },
    {   "real",         TOKfloat80      },

    {   "bool",         TOKbool         },
    {   "char",         TOKchar         },
    {   "wchar",        TOKwchar        },
    {   "dchar",        TOKdchar        },

    {   "ifloat",       TOKimaginary32  },
    {   "idouble",      TOKimaginary64  },
    {   "ireal",        TOKimaginary80  },

    {   "cfloat",       TOKcomplex32    },
    {   "cdouble",      TOKcomplex64    },
    {   "creal",        TOKcomplex80    },

    {   "delegate",     TOKdelegate     },
    {   "function",     TOKfunction     },

    {   "is",           TOKis           },
    {   "if",           TOKif           },
    {   "else",         TOKelse         },
    {   "while",        TOKwhile        },
    {   "for",          TOKfor          },
    {   "do",           TOKdo           },
    {   "switch",       TOKswitch       },
    {   "case",         TOKcase         },
    {   "default",      TOKdefault      },
    {   "break",        TOKbreak        },
    {   "continue",     TOKcontinue     },
    {   "synchronized", TOKsynchronized },
    {   "return",       TOKreturn       },
    {   "goto",         TOKgoto         },
    {   "try",          TOKtry          },
    {   "catch",        TOKcatch        },
    {   "finally",      TOKfinally      },
    {   "with",         TOKwith         },
    {   "asm",          TOKasm          },
    {   "foreach",      TOKforeach      },
    {   "foreach_reverse",      TOKforeach_reverse      },
    {   "scope",        TOKscope        },

    {   "struct",       TOKstruct       },
    {   "class",        TOKclass        },
    {   "interface",    TOKinterface    },
    {   "union",        TOKunion        },
    {   "enum",         TOKenum         },
    {   "import",       TOKimport       },
    {   "mixin",        TOKmixin        },
    {   "static",       TOKstatic       },
    {   "final",        TOKfinal        },
    {   "const",        TOKconst        },
    {   "alias",        TOKalias        },
    {   "override",     TOKoverride     },
    {   "abstract",     TOKabstract     },
    {   "debug",        TOKdebug        },
    {   "deprecated",   TOKdeprecated   },
    {   "in",           TOKin           },
    {   "out",          TOKout          },
    {   "inout",        TOKinout        },
    {   "lazy",         TOKlazy         },
    {   "auto",         TOKauto         },

    {   "align",        TOKalign        },
    {   "extern",       TOKextern       },
    {   "private",      TOKprivate      },
    {   "package",      TOKpackage      },
    {   "protected",    TOKprotected    },
    {   "public",       TOKpublic       },
    {   "export",       TOKexport       },

    {   "invariant",    TOKinvariant    },
    {   "unittest",     TOKunittest     },
    {   "version",      TOKversion      },

    {   "__argTypes",   TOKargTypes     },
    {   "__parameters", TOKparameters   },
    {   "ref",          TOKref          },
    {   "macro",        TOKmacro        },

    {   "pure",         TOKpure         },
    {   "nothrow",      TOKnothrow      },
    {   "__gshared",    TOKgshared      },
    {   "__traits",     TOKtraits       },
    {   "__vector",     TOKvector       },
    {   "__overloadset", TOKoverloadset },
    {   "__FILE__",     TOKfile         },
    {   "__FILE_FULL_PATH__", TOKfilefullpath  },
    {   "__LINE__",     TOKline         },
    {   "__MODULE__",   TOKmodulestring },
    {   "__FUNCTION__", TOKfuncstring   },
    {   "__PRETTY_FUNCTION__", TOKprettyfunc   },
    {   "shared",       TOKshared       },
    {   "immutable",    TOKimmutable    },
    {   NULL,           TOKreserved     }
};

int Token::isKeyword()
{
    for (size_t u = 0; u < nkeywords; u++)
    {
        if (keywords[u].value == value)
            return 1;
    }
    return 0;
}

struct TokenInitializer
{
    TokenInitializer();
};

static TokenInitializer tokeninitializer;

TokenInitializer::TokenInitializer()
{
    Identifier::initTable();
    for (nkeywords = 0; keywords[nkeywords].name; nkeywords++)
    {
        //printf("keyword[%d] = '%s'\n",u, keywords[u].name);
        const char *s = keywords[nkeywords].name;
        size_t len = strlen(s);
        TOK v = keywords[nkeywords].value;
        Identifier::idPool(s, len, v);

        //printf("tochars[%d] = '%s'\n",v, s);
        Token::tochars[v] = s;
    }

    Token::tochars[TOKeof]              = "EOF";
    Token::tochars[TOKlcurly]           = "{";
    Token::tochars[TOKrcurly]           = "}";
    Token::tochars[TOKlparen]           = "(";
    Token::tochars[TOKrparen]           = ")";
    Token::tochars[TOKlbracket]         = "[";
    Token::tochars[TOKrbracket]         = "]";
    Token::tochars[TOKsemicolon]        = ";";
    Token::tochars[TOKcolon]            = ":";
    Token::tochars[TOKcomma]            = ",";
    Token::tochars[TOKdot]              = ".";
    Token::tochars[TOKxor]              = "^";
    Token::tochars[TOKxorass]           = "^=";
    Token::tochars[TOKassign]           = "=";
    Token::tochars[TOKconstruct]        = "=";
    Token::tochars[TOKblit]             = "=";
    Token::tochars[TOKlt]               = "<";
    Token::tochars[TOKgt]               = ">";
    Token::tochars[TOKle]               = "<=";
    Token::tochars[TOKge]               = ">=";
    Token::tochars[TOKequal]            = "==";
    Token::tochars[TOKnotequal]         = "!=";
    Token::tochars[TOKnotidentity]      = "!is";

    Token::tochars[TOKunord]            = "!<>=";
    Token::tochars[TOKue]               = "!<>";
    Token::tochars[TOKlg]               = "<>";
    Token::tochars[TOKleg]              = "<>=";
    Token::tochars[TOKule]              = "!>";
    Token::tochars[TOKul]               = "!>=";
    Token::tochars[TOKuge]              = "!<";
    Token::tochars[TOKug]               = "!<=";

    Token::tochars[TOKnot]              = "!";
    Token::tochars[TOKshl]              = "<<";
    Token::tochars[TOKshr]              = ">>";
    Token::tochars[TOKushr]             = ">>>";
    Token::tochars[TOKadd]              = "+";
    Token::tochars[TOKmin]              = "-";
    Token::tochars[TOKmul]              = "*";
    Token::tochars[TOKdiv]              = "/";
    Token::tochars[TOKmod]              = "%";
    Token::tochars[TOKslice]            = "..";
    Token::tochars[TOKdotdotdot]        = "...";
    Token::tochars[TOKand]              = "&";
    Token::tochars[TOKandand]           = "&&";
    Token::tochars[TOKor]               = "|";
    Token::tochars[TOKoror]             = "||";
    Token::tochars[TOKarray]            = "[]";
    Token::tochars[TOKindex]            = "[i]";
    Token::tochars[TOKaddress]          = "&";
    Token::tochars[TOKstar]             = "*";
    Token::tochars[TOKtilde]            = "~";
    Token::tochars[TOKdollar]           = "$";
    Token::tochars[TOKcast]             = "cast";
    Token::tochars[TOKplusplus]         = "++";
    Token::tochars[TOKminusminus]       = "--";
    Token::tochars[TOKpreplusplus]      = "++";
    Token::tochars[TOKpreminusminus]    = "--";
    Token::tochars[TOKtype]             = "type";
    Token::tochars[TOKquestion]         = "?";
    Token::tochars[TOKneg]              = "-";
    Token::tochars[TOKuadd]             = "+";
    Token::tochars[TOKvar]              = "var";
    Token::tochars[TOKaddass]           = "+=";
    Token::tochars[TOKminass]           = "-=";
    Token::tochars[TOKmulass]           = "*=";
    Token::tochars[TOKdivass]           = "/=";
    Token::tochars[TOKmodass]           = "%=";
    Token::tochars[TOKshlass]           = "<<=";
    Token::tochars[TOKshrass]           = ">>=";
    Token::tochars[TOKushrass]          = ">>>=";
    Token::tochars[TOKandass]           = "&=";
    Token::tochars[TOKorass]            = "|=";
    Token::tochars[TOKcatass]           = "~=";
    Token::tochars[TOKcat]              = "~";
    Token::tochars[TOKcall]             = "call";
    Token::tochars[TOKidentity]         = "is";
    Token::tochars[TOKnotidentity]      = "!is";

    Token::tochars[TOKorass]            = "|=";
    Token::tochars[TOKidentifier]       = "identifier";
    Token::tochars[TOKat]               = "@";
    Token::tochars[TOKpow]              = "^^";
    Token::tochars[TOKpowass]           = "^^=";
    Token::tochars[TOKgoesto]           = "=>";
    Token::tochars[TOKpound]            = "#";

     // For debugging
    Token::tochars[TOKerror]            = "error";
    Token::tochars[TOKdotid]            = "dotid";
    Token::tochars[TOKdottd]            = "dottd";
    Token::tochars[TOKdotti]            = "dotti";
    Token::tochars[TOKdotvar]           = "dotvar";
    Token::tochars[TOKdottype]          = "dottype";
    Token::tochars[TOKsymoff]           = "symoff";
    Token::tochars[TOKarraylength]      = "arraylength";
    Token::tochars[TOKarrayliteral]     = "arrayliteral";
    Token::tochars[TOKassocarrayliteral] = "assocarrayliteral";
    Token::tochars[TOKstructliteral]    = "structliteral";
    Token::tochars[TOKstring]           = "string";
    Token::tochars[TOKdsymbol]          = "symbol";
    Token::tochars[TOKtuple]            = "tuple";
    Token::tochars[TOKdeclaration]      = "declaration";
    Token::tochars[TOKon_scope_exit]    = "scope(exit)";
    Token::tochars[TOKon_scope_success] = "scope(success)";
    Token::tochars[TOKon_scope_failure] = "scope(failure)";
    Token::tochars[TOKdelegateptr]      = "delegateptr";
    Token::tochars[TOKvectorarray]      = "vectorarray";
}
