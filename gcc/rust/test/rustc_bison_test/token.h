/* A Bison parser, made by GNU Bison 3.4.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_TOKEN_H_INCLUDED
#define YY_YY_TOKEN_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
#define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
#define YYTOKENTYPE
enum yytokentype {
    SHL = 258,
    SHR = 259,
    LE = 260,
    EQEQ = 261,
    NE = 262,
    GE = 263,
    ANDAND = 264,
    OROR = 265,
    SHLEQ = 266,
    SHREQ = 267,
    MINUSEQ = 268,
    ANDEQ = 269,
    OREQ = 270,
    PLUSEQ = 271,
    STAREQ = 272,
    SLASHEQ = 273,
    CARETEQ = 274,
    PERCENTEQ = 275,
    DOTDOT = 276,
    DOTDOTDOT = 277,
    MOD_SEP = 278,
    RARROW = 279,
    LARROW = 280,
    FAT_ARROW = 281,
    LIT_BYTE = 282,
    LIT_CHAR = 283,
    LIT_INTEGER = 284,
    LIT_FLOAT = 285,
    LIT_STR = 286,
    LIT_STR_RAW = 287,
    LIT_BYTE_STR = 288,
    LIT_BYTE_STR_RAW = 289,
    IDENT = 290,
    UNDERSCORE = 291,
    LIFETIME = 292,

    // keywords
    SELF = 293,
    STATIC = 294,
    ABSTRACT = 295,
    ALIGNOF = 296,
    AS = 297,
    BECOME = 298,
    BREAK = 299,
    CATCH = 300,
    CRATE = 301,
    DO = 302,
    ELSE = 303,
    ENUM = 304,
    EXTERN = 305,
    FALSE = 306,
    FINAL = 307,
    FN = 308,
    FOR = 309,
    IF = 310,
    IMPL = 311,
    IN = 312,
    LET = 313,
    LOOP = 314,
    MACRO = 315,
    MATCH = 316,
    MOD = 317,
    MOVE = 318,
    MUT = 319,
    OFFSETOF = 320,
    OVERRIDE = 321,
    PRIV = 322,
    PUB = 323,
    PURE = 324,
    REF = 325,
    RETURN = 326,
    SIZEOF = 327,
    STRUCT = 328,
    SUPER = 329,
    UNION = 330,
    UNSIZED = 331,
    TRUE = 332,
    TRAIT = 333,
    TYPE = 334,
    UNSAFE = 335,
    VIRTUAL = 336,
    YIELD = 337,
    DEFAULT = 338,
    USE = 339,
    WHILE = 340,
    CONTINUE = 341,
    PROC = 342,
    BOX = 343,
    CONST = 344,
    WHERE = 345,
    TYPEOF = 346,
    INNER_DOC_COMMENT = 347,
    OUTER_DOC_COMMENT = 348,

    SHEBANG = 349,
    SHEBANG_LINE = 350,
    STATIC_LIFETIME = 351,

    LAMBDA = 352,
    SHIFTPLUS = 353,
    FORTYPE = 354,
    RANGE = 355
};
#endif

/* Value type.  */
#if !defined YYSTYPE && !defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
#define YYSTYPE_IS_TRIVIAL 1
#define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;

int yyparse(void);

#endif /* !YY_YY_TOKEN_H_INCLUDED  */
