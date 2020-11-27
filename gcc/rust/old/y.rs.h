#ifndef __Y_RS_H__
#define __Y_RS_H__

enum yytokentype {
    IMPL = 258,
    AS = 259,
    BREAK = 260,
    CONTINUE = 261,
    RETURN = 262,
    DO = 263,
    DEFUN = 264,
    LET = 265,
    MUT = 266,
    LOOP = 267,
    STATIC = 268,
    RTYPE = 269,
    TYPE_BOOL = 270,
    TYPE_INT = 271,
    TYPE_UINT = 272,
    TYPE_FLOAT = 273,
    ACC = 274,
    ENUM = 275,
    EQUAL_EQUAL = 276,
    NOT_EQUAL = 277,
    LESS = 278,
    GREATER = 279,
    LESS_EQUAL = 280,
    GREATER_EQUAL = 281,
    STRUCT = 282,
    WHILE = 283,
    IF = 284,
    ELSE = 285,
    SELF = 286,
    MATCH = 287,
    GOES = 288,
    XTRUE = 289,
    XFALSE = 290,
    STRING = 291,
    IDENTIFIER = 292,
    INTEGER = 293,
    PUB = 294,
    FOR = 295,
    TRAIT = 296,
    ELIF = 297,
    FLOAT = 298
};

union yystype {
  rdot symbol;
  char * string;
  int integer;
  float ffloat;
  bool boolean;
} ;

extern yystype yylval;

#endif //__Y_RS_H__
