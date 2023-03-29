/**
 * Defines the named entities to support the "\\&amp;Entity;" escape sequence for strings / character literals.
 *
 * Specification $(LINK2 https://dlang.org/spec/entity.html, Named Character Entities)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/entity.d, _entity.d)
 * Documentation:  https://dlang.org/phobos/dmd_entity.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/entity.d
 */

module dmd.entity;

import core.stdc.ctype;

nothrow:

/**********************************
 * See if `name` is an HTML Named Entity
 * Params:
 *      name = name of the entity
 * Returns:
 *      code point corresponding to the named entity
 *      ~0 for not recognized as a named entity
 */
public uint[2] HtmlNamedEntity(scope const char[] name) pure @nogc @safe
{
    const firstC = tolower(name[0]);
    if (firstC >= 'a' && firstC <= 'z')
    {
        // Linear search (use hash table instead?)
        foreach (entity; namesTable[firstC - 'a'])
        {
            if (entity.name == name)
                return [entity.value, entity.value2];
        }
    }
    return [0, 0];
}

private:

/*********************************************
 * Convert from named entity to its encoding.
 * For reference:
 *      https://www.htmlhelp.com/reference/html40/entities/
 *      https://www.w3.org/2003/entities/2007/w3centities-f.ent
 */
struct NameId
{
    string name;
    uint value;
    uint value2;
}

// @todo@ order namesTable and names? by frequency
immutable NameId[][] namesTable =
[
    namesA, namesB, namesC, namesD, namesE, namesF, namesG, namesH, namesI,
    namesJ, namesK, namesL, namesM, namesN, namesO, namesP, namesQ, namesR,
    namesS, namesT, namesU, namesV, namesW, namesX, namesY, namesZ
];

immutable NameId[] namesA =
[
    {"Aacgr",            0x00386},  // GREEK CAPITAL LETTER ALPHA WITH TONOS
    {"aacgr",            0x003AC},  // GREEK SMALL LETTER ALPHA WITH TONOS
    {"Aacute",           0x000C1},  // LATIN CAPITAL LETTER A WITH ACUTE
    {"aacute",           0x000E1},  // LATIN SMALL LETTER A WITH ACUTE
    {"Abreve",           0x00102},  // LATIN CAPITAL LETTER A WITH BREVE
    {"abreve",           0x00103},  // LATIN SMALL LETTER A WITH BREVE
    {"ac",               0x0223E},  // INVERTED LAZY S
    {"acd",              0x0223F},  // SINE WAVE
    {"acE",              0x0223E, 0x00333},  // INVERTED LAZY S with double underline
    {"Acirc",            0x000C2},  // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    {"acirc",            0x000E2},  // LATIN SMALL LETTER A WITH CIRCUMFLEX
    {"acute",            0x000B4},  // ACUTE ACCENT
    {"Acy",              0x00410},  // CYRILLIC CAPITAL LETTER A
    {"acy",              0x00430},  // CYRILLIC SMALL LETTER A
    {"AElig",            0x000C6},  // LATIN CAPITAL LETTER AE
    {"aelig",            0x000E6},  // LATIN SMALL LETTER AE
    {"af",               0x02061},  // FUNCTION APPLICATION
    {"Afr",              0x1D504},  // MATHEMATICAL FRAKTUR CAPITAL A
    {"afr",              0x1D51E},  // MATHEMATICAL FRAKTUR SMALL A
    {"Agr",              0x00391},  // GREEK CAPITAL LETTER ALPHA
    {"agr",              0x003B1},  // GREEK SMALL LETTER ALPHA
    {"Agrave",           0x000C0},  // LATIN CAPITAL LETTER A WITH GRAVE
    {"agrave",           0x000E0},  // LATIN SMALL LETTER A WITH GRAVE
    {"alefsym",          0x02135},  // ALEF SYMBOL
    {"aleph",            0x02135},  // ALEF SYMBOL
    {"Alpha",            0x00391},  // GREEK CAPITAL LETTER ALPHA
    {"alpha",            0x003B1},  // GREEK SMALL LETTER ALPHA
    {"Amacr",            0x00100},  // LATIN CAPITAL LETTER A WITH MACRON
    {"amacr",            0x00101},  // LATIN SMALL LETTER A WITH MACRON
    {"amalg",            0x02A3F},  // AMALGAMATION OR COPRODUCT
    {"amp",              0x00026},  // AMPERSAND
    {"AMP",              0x00026},  // AMPERSAND
    {"and",              0x02227},  // LOGICAL AND
    {"And",              0x02A53},  // DOUBLE LOGICAL AND
    {"andand",           0x02A55},  // TWO INTERSECTING LOGICAL AND
    {"andd",             0x02A5C},  // LOGICAL AND WITH HORIZONTAL DASH
    {"andslope",         0x02A58},  // SLOPING LARGE AND
    {"andv",             0x02A5A},  // LOGICAL AND WITH MIDDLE STEM
    {"ang",              0x02220},  // ANGLE
    {"ange",             0x029A4},  // ANGLE WITH UNDERBAR
    {"angle",            0x02220},  // ANGLE
    {"angmsd",           0x02221},  // MEASURED ANGLE
    {"angmsdaa",         0x029A8},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND RIGHT
    {"angmsdab",         0x029A9},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND LEFT
    {"angmsdac",         0x029AA},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND RIGHT
    {"angmsdad",         0x029AB},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND LEFT
    {"angmsdae",         0x029AC},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND UP
    {"angmsdaf",         0x029AD},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND UP
    {"angmsdag",         0x029AE},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND DOWN
    {"angmsdah",         0x029AF},  // MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND DOWN
    {"angrt",            0x0221F},  // RIGHT ANGLE
    {"angrtvb",          0x022BE},  // RIGHT ANGLE WITH ARC
    {"angrtvbd",         0x0299D},  // MEASURED RIGHT ANGLE WITH DOT
    {"angsph",           0x02222},  // SPHERICAL ANGLE
    {"angst",            0x000C5},  // LATIN CAPITAL LETTER A WITH RING ABOVE
    {"angzarr",          0x0237C},  // RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
    {"Aogon",            0x00104},  // LATIN CAPITAL LETTER A WITH OGONEK
    {"aogon",            0x00105},  // LATIN SMALL LETTER A WITH OGONEK
    {"Aopf",             0x1D538},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL A
    {"aopf",             0x1D552},  // MATHEMATICAL DOUBLE-STRUCK SMALL A
    {"ap",               0x02248},  // ALMOST EQUAL TO
    {"apacir",           0x02A6F},  // ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT
    {"ape",              0x0224A},  // ALMOST EQUAL OR EQUAL TO
    {"apE",              0x02A70},  // APPROXIMATELY EQUAL OR EQUAL TO
    {"apid",             0x0224B},  // TRIPLE TILDE
    {"apos",             0x00027},  // APOSTROPHE
    {"ApplyFunction",    0x02061},  // FUNCTION APPLICATION
    {"approx",           0x02248},  // ALMOST EQUAL TO
    {"approxeq",         0x0224A},  // ALMOST EQUAL OR EQUAL TO
    {"Aring",            0x000C5},  // LATIN CAPITAL LETTER A WITH RING ABOVE
    {"aring",            0x000E5},  // LATIN SMALL LETTER A WITH RING ABOVE
    {"Ascr",             0x1D49C},  // MATHEMATICAL SCRIPT CAPITAL A
    {"ascr",             0x1D4B6},  // MATHEMATICAL SCRIPT SMALL A
    {"Assign",           0x02254},  // COLON EQUALS
    {"ast",              0x0002A},  // ASTERISK
    {"asymp",            0x02248},  // ALMOST EQUAL TO
    {"asympeq",          0x0224D},  // EQUIVALENT TO
    {"Atilde",           0x000C3},  // LATIN CAPITAL LETTER A WITH TILDE
    {"atilde",           0x000E3},  // LATIN SMALL LETTER A WITH TILDE
    {"Auml",             0x000C4},  // LATIN CAPITAL LETTER A WITH DIAERESIS
    {"auml",             0x000E4},  // LATIN SMALL LETTER A WITH DIAERESIS
    {"awconint",         0x02233},  // ANTICLOCKWISE CONTOUR INTEGRAL
    {"awint",            0x02A11},  // ANTICLOCKWISE INTEGRATION
];

immutable NameId[] namesB =
[
    {"backcong",         0x0224C},  // ALL EQUAL TO
    {"backepsilon",      0x003F6},  // GREEK REVERSED LUNATE EPSILON SYMBOL
    {"backprime",        0x02035},  // REVERSED PRIME
    {"backsim",          0x0223D},  // REVERSED TILDE
    {"backsimeq",        0x022CD},  // REVERSED TILDE EQUALS
    {"Backslash",        0x02216},  // SET MINUS
    {"Barv",             0x02AE7},  // SHORT DOWN TACK WITH OVERBAR
    {"barvee",           0x022BD},  // NOR
    {"barwed",           0x02305},  // PROJECTIVE
    {"Barwed",           0x02306},  // PERSPECTIVE
    {"barwedge",         0x02305},  // PROJECTIVE
    {"bbrk",             0x023B5},  // BOTTOM SQUARE BRACKET
    {"bbrktbrk",         0x023B6},  // BOTTOM SQUARE BRACKET OVER TOP SQUARE BRACKET
    {"bcong",            0x0224C},  // ALL EQUAL TO
    {"Bcy",              0x00411},  // CYRILLIC CAPITAL LETTER BE
    {"bcy",              0x00431},  // CYRILLIC SMALL LETTER BE
    {"bdquo",            0x0201E},  // DOUBLE LOW-9 QUOTATION MARK
    {"becaus",           0x02235},  // BECAUSE
    {"because",          0x02235},  // BECAUSE
    {"Because",          0x02235},  // BECAUSE
    {"bemptyv",          0x029B0},  // REVERSED EMPTY SET
    {"bepsi",            0x003F6},  // GREEK REVERSED LUNATE EPSILON SYMBOL
    {"bernou",           0x0212C},  // SCRIPT CAPITAL B
    {"Bernoullis",       0x0212C},  // SCRIPT CAPITAL B
    {"Beta",             0x00392},  // GREEK CAPITAL LETTER BETA
    {"beta",             0x003B2},  // GREEK SMALL LETTER BETA
    {"beth",             0x02136},  // BET SYMBOL
    {"between",          0x0226C},  // BETWEEN
    {"Bfr",              0x1D505},  // MATHEMATICAL FRAKTUR CAPITAL B
    {"bfr",              0x1D51F},  // MATHEMATICAL FRAKTUR SMALL B
    {"Bgr",              0x00392},  // GREEK CAPITAL LETTER BETA
    {"bgr",              0x003B2},  // GREEK SMALL LETTER BETA
    {"bigcap",           0x022C2},  // N-ARY INTERSECTION
    {"bigcirc",          0x025EF},  // LARGE CIRCLE
    {"bigcup",           0x022C3},  // N-ARY UNION
    {"bigodot",          0x02A00},  // N-ARY CIRCLED DOT OPERATOR
    {"bigoplus",         0x02A01},  // N-ARY CIRCLED PLUS OPERATOR
    {"bigotimes",        0x02A02},  // N-ARY CIRCLED TIMES OPERATOR
    {"bigsqcup",         0x02A06},  // N-ARY SQUARE UNION OPERATOR
    {"bigstar",          0x02605},  // BLACK STAR
    {"bigtriangledown",  0x025BD},  // WHITE DOWN-POINTING TRIANGLE
    {"bigtriangleup",    0x025B3},  // WHITE UP-POINTING TRIANGLE
    {"biguplus",         0x02A04},  // N-ARY UNION OPERATOR WITH PLUS
    {"bigvee",           0x022C1},  // N-ARY LOGICAL OR
    {"bigwedge",         0x022C0},  // N-ARY LOGICAL AND
    {"bkarow",           0x0290D},  // RIGHTWARDS DOUBLE DASH ARROW
    {"blacklozenge",     0x029EB},  // BLACK LOZENGE
    {"blacksquare",      0x025AA},  // BLACK SMALL SQUARE
    {"blacktriangle",    0x025B4},  // BLACK UP-POINTING SMALL TRIANGLE
    {"blacktriangledown", 0x025BE},  // BLACK DOWN-POINTING SMALL TRIANGLE
    {"blacktriangleleft", 0x025C2},  // BLACK LEFT-POINTING SMALL TRIANGLE
    {"blacktriangleright", 0x025B8},  // BLACK RIGHT-POINTING SMALL TRIANGLE
    {"blank",            0x02423},  // OPEN BOX
    {"blk12",            0x02592},  // MEDIUM SHADE
    {"blk14",            0x02591},  // LIGHT SHADE
    {"blk34",            0x02593},  // DARK SHADE
    {"block",            0x02588},  // FULL BLOCK
    {"bne",              0x0003D, 0x020E5},  // EQUALS SIGN with reverse slash
    {"bnequiv",          0x02261, 0x020E5},  // IDENTICAL TO with reverse slash
    {"bnot",             0x02310},  // REVERSED NOT SIGN
    {"bNot",             0x02AED},  // REVERSED DOUBLE STROKE NOT SIGN
    {"Bopf",             0x1D539},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL B
    {"bopf",             0x1D553},  // MATHEMATICAL DOUBLE-STRUCK SMALL B
    {"bot",              0x022A5},  // UP TACK
    {"bottom",           0x022A5},  // UP TACK
    {"bowtie",           0x022C8},  // BOWTIE
    {"boxbox",           0x029C9},  // TWO JOINED SQUARES
    {"boxdl",            0x02510},  // BOX DRAWINGS LIGHT DOWN AND LEFT
    {"boxdL",            0x02555},  // BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    {"boxDl",            0x02556},  // BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    {"boxDL",            0x02557},  // BOX DRAWINGS DOUBLE DOWN AND LEFT
    {"boxdr",            0x0250C},  // BOX DRAWINGS LIGHT DOWN AND RIGHT
    {"boxdR",            0x02552},  // BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    {"boxDr",            0x02553},  // BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    {"boxDR",            0x02554},  // BOX DRAWINGS DOUBLE DOWN AND RIGHT
    {"boxh",             0x02500},  // BOX DRAWINGS LIGHT HORIZONTAL
    {"boxH",             0x02550},  // BOX DRAWINGS DOUBLE HORIZONTAL
    {"boxhd",            0x0252C},  // BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    {"boxHd",            0x02564},  // BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    {"boxhD",            0x02565},  // BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    {"boxHD",            0x02566},  // BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    {"boxhu",            0x02534},  // BOX DRAWINGS LIGHT UP AND HORIZONTAL
    {"boxHu",            0x02567},  // BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    {"boxhU",            0x02568},  // BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    {"boxHU",            0x02569},  // BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    {"boxminus",         0x0229F},  // SQUARED MINUS
    {"boxplus",          0x0229E},  // SQUARED PLUS
    {"boxtimes",         0x022A0},  // SQUARED TIMES
    {"boxul",            0x02518},  // BOX DRAWINGS LIGHT UP AND LEFT
    {"boxuL",            0x0255B},  // BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    {"boxUl",            0x0255C},  // BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    {"boxUL",            0x0255D},  // BOX DRAWINGS DOUBLE UP AND LEFT
    {"boxur",            0x02514},  // BOX DRAWINGS LIGHT UP AND RIGHT
    {"boxuR",            0x02558},  // BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    {"boxUr",            0x02559},  // BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    {"boxUR",            0x0255A},  // BOX DRAWINGS DOUBLE UP AND RIGHT
    {"boxv",             0x02502},  // BOX DRAWINGS LIGHT VERTICAL
    {"boxV",             0x02551},  // BOX DRAWINGS DOUBLE VERTICAL
    {"boxvh",            0x0253C},  // BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    {"boxvH",            0x0256A},  // BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    {"boxVh",            0x0256B},  // BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    {"boxVH",            0x0256C},  // BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    {"boxvl",            0x02524},  // BOX DRAWINGS LIGHT VERTICAL AND LEFT
    {"boxvL",            0x02561},  // BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    {"boxVl",            0x02562},  // BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    {"boxVL",            0x02563},  // BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    {"boxvr",            0x0251C},  // BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    {"boxvR",            0x0255E},  // BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    {"boxVr",            0x0255F},  // BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    {"boxVR",            0x02560},  // BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    {"bprime",           0x02035},  // REVERSED PRIME
    {"breve",            0x002D8},  // BREVE
    {"Breve",            0x002D8},  // BREVE
    {"brvbar",           0x000A6},  // BROKEN BAR
    {"Bscr",             0x0212C},  // SCRIPT CAPITAL B
    {"bscr",             0x1D4B7},  // MATHEMATICAL SCRIPT SMALL B
    {"bsemi",            0x0204F},  // REVERSED SEMICOLON
    {"bsim",             0x0223D},  // REVERSED TILDE
    {"bsime",            0x022CD},  // REVERSED TILDE EQUALS
    {"bsol",             0x0005C},  // REVERSE SOLIDUS
    {"bsolb",            0x029C5},  // SQUARED FALLING DIAGONAL SLASH
    {"bsolhsub",         0x027C8},  // REVERSE SOLIDUS PRECEDING SUBSET
    {"bull",             0x02022},  // BULLET
    {"bullet",           0x02022},  // BULLET
    {"bump",             0x0224E},  // GEOMETRICALLY EQUIVALENT TO
    {"bumpe",            0x0224F},  // DIFFERENCE BETWEEN
    {"bumpE",            0x02AAE},  // EQUALS SIGN WITH BUMPY ABOVE
    {"Bumpeq",           0x0224E},  // GEOMETRICALLY EQUIVALENT TO
    {"bumpeq",           0x0224F},  // DIFFERENCE BETWEEN
];

immutable NameId[] namesC =
[
    {"Cacute",           0x00106},  // LATIN CAPITAL LETTER C WITH ACUTE
    {"cacute",           0x00107},  // LATIN SMALL LETTER C WITH ACUTE
    {"cap",              0x02229},  // INTERSECTION
    {"Cap",              0x022D2},  // DOUBLE INTERSECTION
    {"capand",           0x02A44},  // INTERSECTION WITH LOGICAL AND
    {"capbrcup",         0x02A49},  // INTERSECTION ABOVE BAR ABOVE UNION
    {"capcap",           0x02A4B},  // INTERSECTION BESIDE AND JOINED WITH INTERSECTION
    {"capcup",           0x02A47},  // INTERSECTION ABOVE UNION
    {"capdot",           0x02A40},  // INTERSECTION WITH DOT
    {"CapitalDifferentialD", 0x02145},  // DOUBLE-STRUCK ITALIC CAPITAL D
    {"caps",             0x02229, 0x0FE00},  // INTERSECTION with serifs
    {"caret",            0x02041},  // CARET INSERTION POINT
    {"caron",            0x002C7},  // CARON
    {"Cayleys",          0x0212D},  // BLACK-LETTER CAPITAL C
    {"ccaps",            0x02A4D},  // CLOSED INTERSECTION WITH SERIFS
    {"Ccaron",           0x0010C},  // LATIN CAPITAL LETTER C WITH CARON
    {"ccaron",           0x0010D},  // LATIN SMALL LETTER C WITH CARON
    {"Ccedil",           0x000C7},  // LATIN CAPITAL LETTER C WITH CEDILLA
    {"ccedil",           0x000E7},  // LATIN SMALL LETTER C WITH CEDILLA
    {"Ccirc",            0x00108},  // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    {"ccirc",            0x00109},  // LATIN SMALL LETTER C WITH CIRCUMFLEX
    {"Cconint",          0x02230},  // VOLUME INTEGRAL
    {"ccups",            0x02A4C},  // CLOSED UNION WITH SERIFS
    {"ccupssm",          0x02A50},  // CLOSED UNION WITH SERIFS AND SMASH PRODUCT
    {"Cdot",             0x0010A},  // LATIN CAPITAL LETTER C WITH DOT ABOVE
    {"cdot",             0x0010B},  // LATIN SMALL LETTER C WITH DOT ABOVE
    {"cedil",            0x000B8},  // CEDILLA
    {"Cedilla",          0x000B8},  // CEDILLA
    {"cemptyv",          0x029B2},  // EMPTY SET WITH SMALL CIRCLE ABOVE
    {"cent",             0x000A2},  // CENT SIGN
    {"centerdot",        0x000B7},  // MIDDLE DOT
    {"CenterDot",        0x000B7},  // MIDDLE DOT
    {"Cfr",              0x0212D},  // BLACK-LETTER CAPITAL C
    {"cfr",              0x1D520},  // MATHEMATICAL FRAKTUR SMALL C
    {"CHcy",             0x00427},  // CYRILLIC CAPITAL LETTER CHE
    {"chcy",             0x00447},  // CYRILLIC SMALL LETTER CHE
    {"check",            0x02713},  // CHECK MARK
    {"checkmark",        0x02713},  // CHECK MARK
    {"Chi",              0x003A7},  // GREEK CAPITAL LETTER CHI
    {"chi",              0x003C7},  // GREEK SMALL LETTER CHI
    {"cir",              0x025CB},  // WHITE CIRCLE
    {"circ",             0x002C6},  // MODIFIER LETTER CIRCUMFLEX ACCENT
    {"circeq",           0x02257},  // RING EQUAL TO
    {"circlearrowleft",  0x021BA},  // ANTICLOCKWISE OPEN CIRCLE ARROW
    {"circlearrowright", 0x021BB},  // CLOCKWISE OPEN CIRCLE ARROW
    {"circledast",       0x0229B},  // CIRCLED ASTERISK OPERATOR
    {"circledcirc",      0x0229A},  // CIRCLED RING OPERATOR
    {"circleddash",      0x0229D},  // CIRCLED DASH
    {"CircleDot",        0x02299},  // CIRCLED DOT OPERATOR
    {"circledR",         0x000AE},  // REGISTERED SIGN
    {"circledS",         0x024C8},  // CIRCLED LATIN CAPITAL LETTER S
    {"CircleMinus",      0x02296},  // CIRCLED MINUS
    {"CirclePlus",       0x02295},  // CIRCLED PLUS
    {"CircleTimes",      0x02297},  // CIRCLED TIMES
    {"cire",             0x02257},  // RING EQUAL TO
    {"cirE",             0x029C3},  // CIRCLE WITH TWO HORIZONTAL STROKES TO THE RIGHT
    {"cirfnint",         0x02A10},  // CIRCULATION FUNCTION
    {"cirmid",           0x02AEF},  // VERTICAL LINE WITH CIRCLE ABOVE
    {"cirscir",          0x029C2},  // CIRCLE WITH SMALL CIRCLE TO THE RIGHT
    {"ClockwiseContourIntegral", 0x02232},  // CLOCKWISE CONTOUR INTEGRAL
    {"CloseCurlyDoubleQuote", 0x0201D},  // RIGHT DOUBLE QUOTATION MARK
    {"CloseCurlyQuote",  0x02019},  // RIGHT SINGLE QUOTATION MARK
    {"clubs",            0x02663},  // BLACK CLUB SUIT
    {"clubsuit",         0x02663},  // BLACK CLUB SUIT
    {"colon",            0x0003A},  // COLON
    {"Colon",            0x02237},  // PROPORTION
    {"colone",           0x02254},  // COLON EQUALS
    {"Colone",           0x02A74},  // DOUBLE COLON EQUAL
    {"coloneq",          0x02254},  // COLON EQUALS
    {"comma",            0x0002C},  // COMMA
    {"commat",           0x00040},  // COMMERCIAL AT
    {"comp",             0x02201},  // COMPLEMENT
    {"compfn",           0x02218},  // RING OPERATOR
    {"complement",       0x02201},  // COMPLEMENT
    {"complexes",        0x02102},  // DOUBLE-STRUCK CAPITAL C
    {"cong",             0x02245},  // APPROXIMATELY EQUAL TO
    {"congdot",          0x02A6D},  // CONGRUENT WITH DOT ABOVE
    {"Congruent",        0x02261},  // IDENTICAL TO
    {"conint",           0x0222E},  // CONTOUR INTEGRAL
    {"Conint",           0x0222F},  // SURFACE INTEGRAL
    {"ContourIntegral",  0x0222E},  // CONTOUR INTEGRAL
    {"Copf",             0x02102},  // DOUBLE-STRUCK CAPITAL C
    {"copf",             0x1D554},  // MATHEMATICAL DOUBLE-STRUCK SMALL C
    {"coprod",           0x02210},  // N-ARY COPRODUCT
    {"Coproduct",        0x02210},  // N-ARY COPRODUCT
    {"copy",             0x000A9},  // COPYRIGHT SIGN
    {"COPY",             0x000A9},  // COPYRIGHT SIGN
    {"copysr",           0x02117},  // SOUND RECORDING COPYRIGHT
    {"CounterClockwiseContourIntegral", 0x02233},  // ANTICLOCKWISE CONTOUR INTEGRAL
    {"crarr",            0x021B5},  // DOWNWARDS ARROW WITH CORNER LEFTWARDS
    {"cross",            0x02717},  // BALLOT X
    {"Cross",            0x02A2F},  // VECTOR OR CROSS PRODUCT
    {"Cscr",             0x1D49E},  // MATHEMATICAL SCRIPT CAPITAL C
    {"cscr",             0x1D4B8},  // MATHEMATICAL SCRIPT SMALL C
    {"csub",             0x02ACF},  // CLOSED SUBSET
    {"csube",            0x02AD1},  // CLOSED SUBSET OR EQUAL TO
    {"csup",             0x02AD0},  // CLOSED SUPERSET
    {"csupe",            0x02AD2},  // CLOSED SUPERSET OR EQUAL TO
    {"ctdot",            0x022EF},  // MIDLINE HORIZONTAL ELLIPSIS
    {"cudarrl",          0x02938},  // RIGHT-SIDE ARC CLOCKWISE ARROW
    {"cudarrr",          0x02935},  // ARROW POINTING RIGHTWARDS THEN CURVING DOWNWARDS
    {"cuepr",            0x022DE},  // EQUAL TO OR PRECEDES
    {"cuesc",            0x022DF},  // EQUAL TO OR SUCCEEDS
    {"cularr",           0x021B6},  // ANTICLOCKWISE TOP SEMICIRCLE ARROW
    {"cularrp",          0x0293D},  // TOP ARC ANTICLOCKWISE ARROW WITH PLUS
    {"cup",              0x0222A},  // UNION
    {"Cup",              0x022D3},  // DOUBLE UNION
    {"cupbrcap",         0x02A48},  // UNION ABOVE BAR ABOVE INTERSECTION
    {"CupCap",           0x0224D},  // EQUIVALENT TO
    {"cupcap",           0x02A46},  // UNION ABOVE INTERSECTION
    {"cupcup",           0x02A4A},  // UNION BESIDE AND JOINED WITH UNION
    {"cupdot",           0x0228D},  // MULTISET MULTIPLICATION
    {"cupor",            0x02A45},  // UNION WITH LOGICAL OR
    {"cups",             0x0222A, 0x0FE00},  // UNION with serifs
    {"curarr",           0x021B7},  // CLOCKWISE TOP SEMICIRCLE ARROW
    {"curarrm",          0x0293C},  // TOP ARC CLOCKWISE ARROW WITH MINUS
    {"curlyeqprec",      0x022DE},  // EQUAL TO OR PRECEDES
    {"curlyeqsucc",      0x022DF},  // EQUAL TO OR SUCCEEDS
    {"curlyvee",         0x022CE},  // CURLY LOGICAL OR
    {"curlywedge",       0x022CF},  // CURLY LOGICAL AND
    {"curren",           0x000A4},  // CURRENCY SIGN
    {"curvearrowleft",   0x021B6},  // ANTICLOCKWISE TOP SEMICIRCLE ARROW
    {"curvearrowright",  0x021B7},  // CLOCKWISE TOP SEMICIRCLE ARROW
    {"cuvee",            0x022CE},  // CURLY LOGICAL OR
    {"cuwed",            0x022CF},  // CURLY LOGICAL AND
    {"cwconint",         0x02232},  // CLOCKWISE CONTOUR INTEGRAL
    {"cwint",            0x02231},  // CLOCKWISE INTEGRAL
    {"cylcty",           0x0232D},  // CYLINDRICITY
];

immutable NameId[] namesD =
[
    {"dagger",           0x02020},  // DAGGER
    {"Dagger",           0x02021},  // DOUBLE DAGGER
    {"daleth",           0x02138},  // DALET SYMBOL
    {"darr",             0x02193},  // DOWNWARDS ARROW
    {"Darr",             0x021A1},  // DOWNWARDS TWO HEADED ARROW
    {"dArr",             0x021D3},  // DOWNWARDS DOUBLE ARROW
    {"dash",             0x02010},  // HYPHEN
    {"dashv",            0x022A3},  // LEFT TACK
    {"Dashv",            0x02AE4},  // VERTICAL BAR DOUBLE LEFT TURNSTILE
    {"dbkarow",          0x0290F},  // RIGHTWARDS TRIPLE DASH ARROW
    {"dblac",            0x002DD},  // DOUBLE ACUTE ACCENT
    {"Dcaron",           0x0010E},  // LATIN CAPITAL LETTER D WITH CARON
    {"dcaron",           0x0010F},  // LATIN SMALL LETTER D WITH CARON
    {"Dcy",              0x00414},  // CYRILLIC CAPITAL LETTER DE
    {"dcy",              0x00434},  // CYRILLIC SMALL LETTER DE
    {"DD",               0x02145},  // DOUBLE-STRUCK ITALIC CAPITAL D
    {"dd",               0x02146},  // DOUBLE-STRUCK ITALIC SMALL D
    {"ddagger",          0x02021},  // DOUBLE DAGGER
    {"ddarr",            0x021CA},  // DOWNWARDS PAIRED ARROWS
    {"DDotrahd",         0x02911},  // RIGHTWARDS ARROW WITH DOTTED STEM
    {"ddotseq",          0x02A77},  // EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW
    {"deg",              0x000B0},  // DEGREE SIGN
    {"Del",              0x02207},  // NABLA
    {"Delta",            0x00394},  // GREEK CAPITAL LETTER DELTA
    {"delta",            0x003B4},  // GREEK SMALL LETTER DELTA
    {"demptyv",          0x029B1},  // EMPTY SET WITH OVERBAR
    {"dfisht",           0x0297F},  // DOWN FISH TAIL
    {"Dfr",              0x1D507},  // MATHEMATICAL FRAKTUR CAPITAL D
    {"dfr",              0x1D521},  // MATHEMATICAL FRAKTUR SMALL D
    {"Dgr",              0x00394},  // GREEK CAPITAL LETTER DELTA
    {"dgr",              0x003B4},  // GREEK SMALL LETTER DELTA
    {"dHar",             0x02965},  // DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT
    {"dharl",            0x021C3},  // DOWNWARDS HARPOON WITH BARB LEFTWARDS
    {"dharr",            0x021C2},  // DOWNWARDS HARPOON WITH BARB RIGHTWARDS
    {"DiacriticalAcute", 0x000B4},  // ACUTE ACCENT
    {"DiacriticalDot",   0x002D9},  // DOT ABOVE
    {"DiacriticalDoubleAcute", 0x002DD},  // DOUBLE ACUTE ACCENT
    {"DiacriticalGrave", 0x00060},  // GRAVE ACCENT
    {"DiacriticalTilde", 0x002DC},  // SMALL TILDE
    {"diam",             0x022C4},  // DIAMOND OPERATOR
    {"diamond",          0x022C4},  // DIAMOND OPERATOR
    {"Diamond",          0x022C4},  // DIAMOND OPERATOR
    {"diamondsuit",      0x02666},  // BLACK DIAMOND SUIT
    {"diams",            0x02666},  // BLACK DIAMOND SUIT
    {"die",              0x000A8},  // DIAERESIS
    {"DifferentialD",    0x02146},  // DOUBLE-STRUCK ITALIC SMALL D
    {"digamma",          0x003DD},  // GREEK SMALL LETTER DIGAMMA
    {"disin",            0x022F2},  // ELEMENT OF WITH LONG HORIZONTAL STROKE
    {"div",              0x000F7},  // DIVISION SIGN
    {"divide",           0x000F7},  // DIVISION SIGN
    {"divideontimes",    0x022C7},  // DIVISION TIMES
    {"divonx",           0x022C7},  // DIVISION TIMES
    {"DJcy",             0x00402},  // CYRILLIC CAPITAL LETTER DJE
    {"djcy",             0x00452},  // CYRILLIC SMALL LETTER DJE
    {"dlcorn",           0x0231E},  // BOTTOM LEFT CORNER
    {"dlcrop",           0x0230D},  // BOTTOM LEFT CROP
    {"dollar",           0x00024},  // DOLLAR SIGN
    {"Dopf",             0x1D53B},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL D
    {"dopf",             0x1D555},  // MATHEMATICAL DOUBLE-STRUCK SMALL D
    {"Dot",              0x000A8},  // DIAERESIS
    {"dot",              0x002D9},  // DOT ABOVE
    {"DotDot",           0x020DC},  // COMBINING FOUR DOTS ABOVE
    {"doteq",            0x02250},  // APPROACHES THE LIMIT
    {"doteqdot",         0x02251},  // GEOMETRICALLY EQUAL TO
    {"DotEqual",         0x02250},  // APPROACHES THE LIMIT
    {"dotminus",         0x02238},  // DOT MINUS
    {"dotplus",          0x02214},  // DOT PLUS
    {"dotsquare",        0x022A1},  // SQUARED DOT OPERATOR
    {"doublebarwedge",   0x02306},  // PERSPECTIVE
    {"DoubleContourIntegral", 0x0222F},  // SURFACE INTEGRAL
    {"DoubleDot",        0x000A8},  // DIAERESIS
    {"DoubleDownArrow",  0x021D3},  // DOWNWARDS DOUBLE ARROW
    {"DoubleLeftArrow",  0x021D0},  // LEFTWARDS DOUBLE ARROW
    {"DoubleLeftRightArrow", 0x021D4},  // LEFT RIGHT DOUBLE ARROW
    {"DoubleLeftTee",    0x02AE4},  // VERTICAL BAR DOUBLE LEFT TURNSTILE
    {"DoubleLongLeftArrow", 0x027F8},  // LONG LEFTWARDS DOUBLE ARROW
    {"DoubleLongLeftRightArrow", 0x027FA},  // LONG LEFT RIGHT DOUBLE ARROW
    {"DoubleLongRightArrow", 0x027F9},  // LONG RIGHTWARDS DOUBLE ARROW
    {"DoubleRightArrow", 0x021D2},  // RIGHTWARDS DOUBLE ARROW
    {"DoubleRightTee",   0x022A8},  // TRUE
    {"DoubleUpArrow",    0x021D1},  // UPWARDS DOUBLE ARROW
    {"DoubleUpDownArrow", 0x021D5},  // UP DOWN DOUBLE ARROW
    {"DoubleVerticalBar", 0x02225},  // PARALLEL TO
    {"downarrow",        0x02193},  // DOWNWARDS ARROW
    {"DownArrow",        0x02193},  // DOWNWARDS ARROW
    {"Downarrow",        0x021D3},  // DOWNWARDS DOUBLE ARROW
    {"DownArrowBar",     0x02913},  // DOWNWARDS ARROW TO BAR
    {"DownArrowUpArrow", 0x021F5},  // DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW
    {"DownBreve",        0x00311},  // COMBINING INVERTED BREVE
    {"downdownarrows",   0x021CA},  // DOWNWARDS PAIRED ARROWS
    {"downharpoonleft",  0x021C3},  // DOWNWARDS HARPOON WITH BARB LEFTWARDS
    {"downharpoonright", 0x021C2},  // DOWNWARDS HARPOON WITH BARB RIGHTWARDS
    {"DownLeftRightVector", 0x02950},  // LEFT BARB DOWN RIGHT BARB DOWN HARPOON
    {"DownLeftTeeVector", 0x0295E},  // LEFTWARDS HARPOON WITH BARB DOWN FROM BAR
    {"DownLeftVector",   0x021BD},  // LEFTWARDS HARPOON WITH BARB DOWNWARDS
    {"DownLeftVectorBar", 0x02956},  // LEFTWARDS HARPOON WITH BARB DOWN TO BAR
    {"DownRightTeeVector", 0x0295F},  // RIGHTWARDS HARPOON WITH BARB DOWN FROM BAR
    {"DownRightVector",  0x021C1},  // RIGHTWARDS HARPOON WITH BARB DOWNWARDS
    {"DownRightVectorBar", 0x02957},  // RIGHTWARDS HARPOON WITH BARB DOWN TO BAR
    {"DownTee",          0x022A4},  // DOWN TACK
    {"DownTeeArrow",     0x021A7},  // DOWNWARDS ARROW FROM BAR
    {"drbkarow",         0x02910},  // RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW
    {"drcorn",           0x0231F},  // BOTTOM RIGHT CORNER
    {"drcrop",           0x0230C},  // BOTTOM RIGHT CROP
    {"Dscr",             0x1D49F},  // MATHEMATICAL SCRIPT CAPITAL D
    {"dscr",             0x1D4B9},  // MATHEMATICAL SCRIPT SMALL D
    {"DScy",             0x00405},  // CYRILLIC CAPITAL LETTER DZE
    {"dscy",             0x00455},  // CYRILLIC SMALL LETTER DZE
    {"dsol",             0x029F6},  // SOLIDUS WITH OVERBAR
    {"Dstrok",           0x00110},  // LATIN CAPITAL LETTER D WITH STROKE
    {"dstrok",           0x00111},  // LATIN SMALL LETTER D WITH STROKE
    {"dtdot",            0x022F1},  // DOWN RIGHT DIAGONAL ELLIPSIS
    {"dtri",             0x025BF},  // WHITE DOWN-POINTING SMALL TRIANGLE
    {"dtrif",            0x025BE},  // BLACK DOWN-POINTING SMALL TRIANGLE
    {"duarr",            0x021F5},  // DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW
    {"duhar",            0x0296F},  // DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT
    {"dwangle",          0x029A6},  // OBLIQUE ANGLE OPENING UP
    {"DZcy",             0x0040F},  // CYRILLIC CAPITAL LETTER DZHE
    {"dzcy",             0x0045F},  // CYRILLIC SMALL LETTER DZHE
    {"dzigrarr",         0x027FF},  // LONG RIGHTWARDS SQUIGGLE ARROW
];

immutable NameId[] namesE =
[
    {"Eacgr",            0x00388},  // GREEK CAPITAL LETTER EPSILON WITH TONOS
    {"eacgr",            0x003AD},  // GREEK SMALL LETTER EPSILON WITH TONOS
    {"Eacute",           0x000C9},  // LATIN CAPITAL LETTER E WITH ACUTE
    {"eacute",           0x000E9},  // LATIN SMALL LETTER E WITH ACUTE
    {"easter",           0x02A6E},  // EQUALS WITH ASTERISK
    {"Ecaron",           0x0011A},  // LATIN CAPITAL LETTER E WITH CARON
    {"ecaron",           0x0011B},  // LATIN SMALL LETTER E WITH CARON
    {"ecir",             0x02256},  // RING IN EQUAL TO
    {"Ecirc",            0x000CA},  // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    {"ecirc",            0x000EA},  // LATIN SMALL LETTER E WITH CIRCUMFLEX
    {"ecolon",           0x02255},  // EQUALS COLON
    {"Ecy",              0x0042D},  // CYRILLIC CAPITAL LETTER E
    {"ecy",              0x0044D},  // CYRILLIC SMALL LETTER E
    {"eDDot",            0x02A77},  // EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW
    {"Edot",             0x00116},  // LATIN CAPITAL LETTER E WITH DOT ABOVE
    {"edot",             0x00117},  // LATIN SMALL LETTER E WITH DOT ABOVE
    {"eDot",             0x02251},  // GEOMETRICALLY EQUAL TO
    {"ee",               0x02147},  // DOUBLE-STRUCK ITALIC SMALL E
    {"EEacgr",           0x00389},  // GREEK CAPITAL LETTER ETA WITH TONOS
    {"eeacgr",           0x003AE},  // GREEK SMALL LETTER ETA WITH TONOS
    {"EEgr",             0x00397},  // GREEK CAPITAL LETTER ETA
    {"eegr",             0x003B7},  // GREEK SMALL LETTER ETA
    {"efDot",            0x02252},  // APPROXIMATELY EQUAL TO OR THE IMAGE OF
    {"Efr",              0x1D508},  // MATHEMATICAL FRAKTUR CAPITAL E
    {"efr",              0x1D522},  // MATHEMATICAL FRAKTUR SMALL E
    {"eg",               0x02A9A},  // DOUBLE-LINE EQUAL TO OR GREATER-THAN
    {"Egr",              0x00395},  // GREEK CAPITAL LETTER EPSILON
    {"egr",              0x003B5},  // GREEK SMALL LETTER EPSILON
    {"Egrave",           0x000C8},  // LATIN CAPITAL LETTER E WITH GRAVE
    {"egrave",           0x000E8},  // LATIN SMALL LETTER E WITH GRAVE
    {"egs",              0x02A96},  // SLANTED EQUAL TO OR GREATER-THAN
    {"egsdot",           0x02A98},  // SLANTED EQUAL TO OR GREATER-THAN WITH DOT INSIDE
    {"el",               0x02A99},  // DOUBLE-LINE EQUAL TO OR LESS-THAN
    {"Element",          0x02208},  // ELEMENT OF
    {"elinters",         0x023E7},  // ELECTRICAL INTERSECTION
    {"ell",              0x02113},  // SCRIPT SMALL L
    {"els",              0x02A95},  // SLANTED EQUAL TO OR LESS-THAN
    {"elsdot",           0x02A97},  // SLANTED EQUAL TO OR LESS-THAN WITH DOT INSIDE
    {"Emacr",            0x00112},  // LATIN CAPITAL LETTER E WITH MACRON
    {"emacr",            0x00113},  // LATIN SMALL LETTER E WITH MACRON
    {"empty",            0x02205},  // EMPTY SET
    {"emptyset",         0x02205},  // EMPTY SET
    {"EmptySmallSquare", 0x025FB},  // WHITE MEDIUM SQUARE
    {"emptyv",           0x02205},  // EMPTY SET
    {"EmptyVerySmallSquare", 0x025AB},  // WHITE SMALL SQUARE
    {"emsp",             0x02003},  // EM SPACE
    {"emsp13",           0x02004},  // THREE-PER-EM SPACE
    {"emsp14",           0x02005},  // FOUR-PER-EM SPACE
    {"ENG",              0x0014A},  // LATIN CAPITAL LETTER ENG
    {"eng",              0x0014B},  // LATIN SMALL LETTER ENG
    {"ensp",             0x02002},  // EN SPACE
    {"Eogon",            0x00118},  // LATIN CAPITAL LETTER E WITH OGONEK
    {"eogon",            0x00119},  // LATIN SMALL LETTER E WITH OGONEK
    {"Eopf",             0x1D53C},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL E
    {"eopf",             0x1D556},  // MATHEMATICAL DOUBLE-STRUCK SMALL E
    {"epar",             0x022D5},  // EQUAL AND PARALLEL TO
    {"eparsl",           0x029E3},  // EQUALS SIGN AND SLANTED PARALLEL
    {"eplus",            0x02A71},  // EQUALS SIGN ABOVE PLUS SIGN
    {"epsi",             0x003B5},  // GREEK SMALL LETTER EPSILON
    {"Epsilon",          0x00395},  // GREEK CAPITAL LETTER EPSILON
    {"epsilon",          0x003B5},  // GREEK SMALL LETTER EPSILON
    {"epsiv",            0x003F5},  // GREEK LUNATE EPSILON SYMBOL
    {"eqcirc",           0x02256},  // RING IN EQUAL TO
    {"eqcolon",          0x02255},  // EQUALS COLON
    {"eqsim",            0x02242},  // MINUS TILDE
    {"eqslantgtr",       0x02A96},  // SLANTED EQUAL TO OR GREATER-THAN
    {"eqslantless",      0x02A95},  // SLANTED EQUAL TO OR LESS-THAN
    {"Equal",            0x02A75},  // TWO CONSECUTIVE EQUALS SIGNS
    {"equals",           0x0003D},  // EQUALS SIGN
    {"EqualTilde",       0x02242},  // MINUS TILDE
    {"equest",           0x0225F},  // QUESTIONED EQUAL TO
    {"Equilibrium",      0x021CC},  // RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
    {"equiv",            0x02261},  // IDENTICAL TO
    {"equivDD",          0x02A78},  // EQUIVALENT WITH FOUR DOTS ABOVE
    {"eqvparsl",         0x029E5},  // IDENTICAL TO AND SLANTED PARALLEL
    {"erarr",            0x02971},  // EQUALS SIGN ABOVE RIGHTWARDS ARROW
    {"erDot",            0x02253},  // IMAGE OF OR APPROXIMATELY EQUAL TO
    {"escr",             0x0212F},  // SCRIPT SMALL E
    {"Escr",             0x02130},  // SCRIPT CAPITAL E
    {"esdot",            0x02250},  // APPROACHES THE LIMIT
    {"esim",             0x02242},  // MINUS TILDE
    {"Esim",             0x02A73},  // EQUALS SIGN ABOVE TILDE OPERATOR
    {"Eta",              0x00397},  // GREEK CAPITAL LETTER ETA
    {"eta",              0x003B7},  // GREEK SMALL LETTER ETA
    {"ETH",              0x000D0},  // LATIN CAPITAL LETTER ETH
    {"eth",              0x000F0},  // LATIN SMALL LETTER ETH
    {"Euml",             0x000CB},  // LATIN CAPITAL LETTER E WITH DIAERESIS
    {"euml",             0x000EB},  // LATIN SMALL LETTER E WITH DIAERESIS
    {"euro",             0x020AC},  // EURO SIGN
    {"excl",             0x00021},  // EXCLAMATION MARK
    {"exist",            0x02203},  // THERE EXISTS
    {"Exists",           0x02203},  // THERE EXISTS
    {"expectation",      0x02130},  // SCRIPT CAPITAL E
    {"exponentiale",     0x02147},  // DOUBLE-STRUCK ITALIC SMALL E
    {"ExponentialE",     0x02147},  // DOUBLE-STRUCK ITALIC SMALL E
];

immutable NameId[] namesF =
[
    {"fallingdotseq",    0x02252},  // APPROXIMATELY EQUAL TO OR THE IMAGE OF
    {"Fcy",              0x00424},  // CYRILLIC CAPITAL LETTER EF
    {"fcy",              0x00444},  // CYRILLIC SMALL LETTER EF
    {"female",           0x02640},  // FEMALE SIGN
    {"ffilig",           0x0FB03},  // LATIN SMALL LIGATURE FFI
    {"fflig",            0x0FB00},  // LATIN SMALL LIGATURE FF
    {"ffllig",           0x0FB04},  // LATIN SMALL LIGATURE FFL
    {"Ffr",              0x1D509},  // MATHEMATICAL FRAKTUR CAPITAL F
    {"ffr",              0x1D523},  // MATHEMATICAL FRAKTUR SMALL F
    {"filig",            0x0FB01},  // LATIN SMALL LIGATURE FI
    {"FilledSmallSquare", 0x025FC},  // BLACK MEDIUM SQUARE
    {"FilledVerySmallSquare", 0x025AA},  // BLACK SMALL SQUARE
    {"fjlig",            0x00066, 0x0006A},  // fj ligature
    {"flat",             0x0266D},  // MUSIC FLAT SIGN
    {"fllig",            0x0FB02},  // LATIN SMALL LIGATURE FL
    {"fltns",            0x025B1},  // WHITE PARALLELOGRAM
    {"fnof",             0x00192},  // LATIN SMALL LETTER F WITH HOOK
    {"Fopf",             0x1D53D},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL F
    {"fopf",             0x1D557},  // MATHEMATICAL DOUBLE-STRUCK SMALL F
    {"forall",           0x02200},  // FOR ALL
    {"ForAll",           0x02200},  // FOR ALL
    {"fork",             0x022D4},  // PITCHFORK
    {"forkv",            0x02AD9},  // ELEMENT OF OPENING DOWNWARDS
    {"Fouriertrf",       0x02131},  // SCRIPT CAPITAL F
    {"fpartint",         0x02A0D},  // FINITE PART INTEGRAL
    {"frac12",           0x000BD},  // VULGAR FRACTION ONE HALF
    {"frac13",           0x02153},  // VULGAR FRACTION ONE THIRD
    {"frac14",           0x000BC},  // VULGAR FRACTION ONE QUARTER
    {"frac15",           0x02155},  // VULGAR FRACTION ONE FIFTH
    {"frac16",           0x02159},  // VULGAR FRACTION ONE SIXTH
    {"frac18",           0x0215B},  // VULGAR FRACTION ONE EIGHTH
    {"frac23",           0x02154},  // VULGAR FRACTION TWO THIRDS
    {"frac25",           0x02156},  // VULGAR FRACTION TWO FIFTHS
    {"frac34",           0x000BE},  // VULGAR FRACTION THREE QUARTERS
    {"frac35",           0x02157},  // VULGAR FRACTION THREE FIFTHS
    {"frac38",           0x0215C},  // VULGAR FRACTION THREE EIGHTHS
    {"frac45",           0x02158},  // VULGAR FRACTION FOUR FIFTHS
    {"frac56",           0x0215A},  // VULGAR FRACTION FIVE SIXTHS
    {"frac58",           0x0215D},  // VULGAR FRACTION FIVE EIGHTHS
    {"frac78",           0x0215E},  // VULGAR FRACTION SEVEN EIGHTHS
    {"frasl",            0x02044},  // FRACTION SLASH
    {"frown",            0x02322},  // FROWN
    {"Fscr",             0x02131},  // SCRIPT CAPITAL F
    {"fscr",             0x1D4BB},  // MATHEMATICAL SCRIPT SMALL F
];

immutable NameId[] namesG =
[
    {"gacute",           0x001F5},  // LATIN SMALL LETTER G WITH ACUTE
    {"Gamma",            0x00393},  // GREEK CAPITAL LETTER GAMMA
    {"gamma",            0x003B3},  // GREEK SMALL LETTER GAMMA
    {"Gammad",           0x003DC},  // GREEK LETTER DIGAMMA
    {"gammad",           0x003DD},  // GREEK SMALL LETTER DIGAMMA
    {"gap",              0x02A86},  // GREATER-THAN OR APPROXIMATE
    {"Gbreve",           0x0011E},  // LATIN CAPITAL LETTER G WITH BREVE
    {"gbreve",           0x0011F},  // LATIN SMALL LETTER G WITH BREVE
    {"Gcedil",           0x00122},  // LATIN CAPITAL LETTER G WITH CEDILLA
    {"Gcirc",            0x0011C},  // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    {"gcirc",            0x0011D},  // LATIN SMALL LETTER G WITH CIRCUMFLEX
    {"Gcy",              0x00413},  // CYRILLIC CAPITAL LETTER GHE
    {"gcy",              0x00433},  // CYRILLIC SMALL LETTER GHE
    {"Gdot",             0x00120},  // LATIN CAPITAL LETTER G WITH DOT ABOVE
    {"gdot",             0x00121},  // LATIN SMALL LETTER G WITH DOT ABOVE
    {"ge",               0x02265},  // GREATER-THAN OR EQUAL TO
    {"gE",               0x02267},  // GREATER-THAN OVER EQUAL TO
    {"gel",              0x022DB},  // GREATER-THAN EQUAL TO OR LESS-THAN
    {"gEl",              0x02A8C},  // GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN
    {"geq",              0x02265},  // GREATER-THAN OR EQUAL TO
    {"geqq",             0x02267},  // GREATER-THAN OVER EQUAL TO
    {"geqslant",         0x02A7E},  // GREATER-THAN OR SLANTED EQUAL TO
    {"ges",              0x02A7E},  // GREATER-THAN OR SLANTED EQUAL TO
    {"gescc",            0x02AA9},  // GREATER-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL
    {"gesdot",           0x02A80},  // GREATER-THAN OR SLANTED EQUAL TO WITH DOT INSIDE
    {"gesdoto",          0x02A82},  // GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE
    {"gesdotol",         0x02A84},  // GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE LEFT
    {"gesl",             0x022DB, 0x0FE00},  // GREATER-THAN slanted EQUAL TO OR LESS-THAN
    {"gesles",           0x02A94},  // GREATER-THAN ABOVE SLANTED EQUAL ABOVE LESS-THAN ABOVE SLANTED EQUAL
    {"Gfr",              0x1D50A},  // MATHEMATICAL FRAKTUR CAPITAL G
    {"gfr",              0x1D524},  // MATHEMATICAL FRAKTUR SMALL G
    {"gg",               0x0226B},  // MUCH GREATER-THAN
    {"Gg",               0x022D9},  // VERY MUCH GREATER-THAN
    {"ggg",              0x022D9},  // VERY MUCH GREATER-THAN
    {"Ggr",              0x00393},  // GREEK CAPITAL LETTER GAMMA
    {"ggr",              0x003B3},  // GREEK SMALL LETTER GAMMA
    {"gimel",            0x02137},  // GIMEL SYMBOL
    {"GJcy",             0x00403},  // CYRILLIC CAPITAL LETTER GJE
    {"gjcy",             0x00453},  // CYRILLIC SMALL LETTER GJE
    {"gl",               0x02277},  // GREATER-THAN OR LESS-THAN
    {"gla",              0x02AA5},  // GREATER-THAN BESIDE LESS-THAN
    {"glE",              0x02A92},  // GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL
    {"glj",              0x02AA4},  // GREATER-THAN OVERLAPPING LESS-THAN
    {"gnap",             0x02A8A},  // GREATER-THAN AND NOT APPROXIMATE
    {"gnapprox",         0x02A8A},  // GREATER-THAN AND NOT APPROXIMATE
    {"gnE",              0x02269},  // GREATER-THAN BUT NOT EQUAL TO
    {"gne",              0x02A88},  // GREATER-THAN AND SINGLE-LINE NOT EQUAL TO
    {"gneq",             0x02A88},  // GREATER-THAN AND SINGLE-LINE NOT EQUAL TO
    {"gneqq",            0x02269},  // GREATER-THAN BUT NOT EQUAL TO
    {"gnsim",            0x022E7},  // GREATER-THAN BUT NOT EQUIVALENT TO
    {"Gopf",             0x1D53E},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL G
    {"gopf",             0x1D558},  // MATHEMATICAL DOUBLE-STRUCK SMALL G
    {"grave",            0x00060},  // GRAVE ACCENT
    {"GreaterEqual",     0x02265},  // GREATER-THAN OR EQUAL TO
    {"GreaterEqualLess", 0x022DB},  // GREATER-THAN EQUAL TO OR LESS-THAN
    {"GreaterFullEqual", 0x02267},  // GREATER-THAN OVER EQUAL TO
    {"GreaterGreater",   0x02AA2},  // DOUBLE NESTED GREATER-THAN
    {"GreaterLess",      0x02277},  // GREATER-THAN OR LESS-THAN
    {"GreaterSlantEqual", 0x02A7E},  // GREATER-THAN OR SLANTED EQUAL TO
    {"GreaterTilde",     0x02273},  // GREATER-THAN OR EQUIVALENT TO
    {"gscr",             0x0210A},  // SCRIPT SMALL G
    {"Gscr",             0x1D4A2},  // MATHEMATICAL SCRIPT CAPITAL G
    {"gsim",             0x02273},  // GREATER-THAN OR EQUIVALENT TO
    {"gsime",            0x02A8E},  // GREATER-THAN ABOVE SIMILAR OR EQUAL
    {"gsiml",            0x02A90},  // GREATER-THAN ABOVE SIMILAR ABOVE LESS-THAN
    {"gt",               0x0003E},  // GREATER-THAN SIGN
    {"GT",               0x0003E},  // GREATER-THAN SIGN
    {"Gt",               0x0226B},  // MUCH GREATER-THAN
    {"gtcc",             0x02AA7},  // GREATER-THAN CLOSED BY CURVE
    {"gtcir",            0x02A7A},  // GREATER-THAN WITH CIRCLE INSIDE
    {"gtdot",            0x022D7},  // GREATER-THAN WITH DOT
    {"gtlPar",           0x02995},  // DOUBLE LEFT ARC GREATER-THAN BRACKET
    {"gtquest",          0x02A7C},  // GREATER-THAN WITH QUESTION MARK ABOVE
    {"gtrapprox",        0x02A86},  // GREATER-THAN OR APPROXIMATE
    {"gtrarr",           0x02978},  // GREATER-THAN ABOVE RIGHTWARDS ARROW
    {"gtrdot",           0x022D7},  // GREATER-THAN WITH DOT
    {"gtreqless",        0x022DB},  // GREATER-THAN EQUAL TO OR LESS-THAN
    {"gtreqqless",       0x02A8C},  // GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN
    {"gtrless",          0x02277},  // GREATER-THAN OR LESS-THAN
    {"gtrsim",           0x02273},  // GREATER-THAN OR EQUIVALENT TO
    {"gvertneqq",        0x02269, 0x0FE00},  // GREATER-THAN BUT NOT EQUAL TO - with vertical stroke
    {"gvnE",             0x02269, 0x0FE00},  // GREATER-THAN BUT NOT EQUAL TO - with vertical stroke
];

immutable NameId[] namesH =
[
    {"Hacek",            0x002C7},  // CARON
    {"hairsp",           0x0200A},  // HAIR SPACE
    {"half",             0x000BD},  // VULGAR FRACTION ONE HALF
    {"hamilt",           0x0210B},  // SCRIPT CAPITAL H
    {"HARDcy",           0x0042A},  // CYRILLIC CAPITAL LETTER HARD SIGN
    {"hardcy",           0x0044A},  // CYRILLIC SMALL LETTER HARD SIGN
    {"harr",             0x02194},  // LEFT RIGHT ARROW
    {"hArr",             0x021D4},  // LEFT RIGHT DOUBLE ARROW
    {"harrcir",          0x02948},  // LEFT RIGHT ARROW THROUGH SMALL CIRCLE
    {"harrw",            0x021AD},  // LEFT RIGHT WAVE ARROW
    {"Hat",              0x0005E},  // CIRCUMFLEX ACCENT
    {"hbar",             0x0210F},  // PLANCK CONSTANT OVER TWO PI
    {"Hcirc",            0x00124},  // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    {"hcirc",            0x00125},  // LATIN SMALL LETTER H WITH CIRCUMFLEX
    {"hearts",           0x02665},  // BLACK HEART SUIT
    {"heartsuit",        0x02665},  // BLACK HEART SUIT
    {"hellip",           0x02026},  // HORIZONTAL ELLIPSIS
    {"hercon",           0x022B9},  // HERMITIAN CONJUGATE MATRIX
    {"Hfr",              0x0210C},  // BLACK-LETTER CAPITAL H
    {"hfr",              0x1D525},  // MATHEMATICAL FRAKTUR SMALL H
    {"HilbertSpace",     0x0210B},  // SCRIPT CAPITAL H
    {"hksearow",         0x02925},  // SOUTH EAST ARROW WITH HOOK
    {"hkswarow",         0x02926},  // SOUTH WEST ARROW WITH HOOK
    {"hoarr",            0x021FF},  // LEFT RIGHT OPEN-HEADED ARROW
    {"homtht",           0x0223B},  // HOMOTHETIC
    {"hookleftarrow",    0x021A9},  // LEFTWARDS ARROW WITH HOOK
    {"hookrightarrow",   0x021AA},  // RIGHTWARDS ARROW WITH HOOK
    {"Hopf",             0x0210D},  // DOUBLE-STRUCK CAPITAL H
    {"hopf",             0x1D559},  // MATHEMATICAL DOUBLE-STRUCK SMALL H
    {"horbar",           0x02015},  // HORIZONTAL BAR
    {"HorizontalLine",   0x02500},  // BOX DRAWINGS LIGHT HORIZONTAL
    {"Hscr",             0x0210B},  // SCRIPT CAPITAL H
    {"hscr",             0x1D4BD},  // MATHEMATICAL SCRIPT SMALL H
    {"hslash",           0x0210F},  // PLANCK CONSTANT OVER TWO PI
    {"Hstrok",           0x00126},  // LATIN CAPITAL LETTER H WITH STROKE
    {"hstrok",           0x00127},  // LATIN SMALL LETTER H WITH STROKE
    {"HumpDownHump",     0x0224E},  // GEOMETRICALLY EQUIVALENT TO
    {"HumpEqual",        0x0224F},  // DIFFERENCE BETWEEN
    {"hybull",           0x02043},  // HYPHEN BULLET
    {"hyphen",           0x02010},  // HYPHEN
];

immutable NameId[] namesI =
[
    {"Iacgr",            0x0038A},  // GREEK CAPITAL LETTER IOTA WITH TONOS
    {"iacgr",            0x003AF},  // GREEK SMALL LETTER IOTA WITH TONOS
    {"Iacute",           0x000CD},  // LATIN CAPITAL LETTER I WITH ACUTE
    {"iacute",           0x000ED},  // LATIN SMALL LETTER I WITH ACUTE
    {"ic",               0x02063},  // INVISIBLE SEPARATOR
    {"Icirc",            0x000CE},  // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    {"icirc",            0x000EE},  // LATIN SMALL LETTER I WITH CIRCUMFLEX
    {"Icy",              0x00418},  // CYRILLIC CAPITAL LETTER I
    {"icy",              0x00438},  // CYRILLIC SMALL LETTER I
    {"idiagr",           0x00390},  // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    {"Idigr",            0x003AA},  // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    {"idigr",            0x003CA},  // GREEK SMALL LETTER IOTA WITH DIALYTIKA
    {"Idot",             0x00130},  // LATIN CAPITAL LETTER I WITH DOT ABOVE
    {"IEcy",             0x00415},  // CYRILLIC CAPITAL LETTER IE
    {"iecy",             0x00435},  // CYRILLIC SMALL LETTER IE
    {"iexcl",            0x000A1},  // INVERTED EXCLAMATION MARK
    {"iff",              0x021D4},  // LEFT RIGHT DOUBLE ARROW
    {"Ifr",              0x02111},  // BLACK-LETTER CAPITAL I
    {"ifr",              0x1D526},  // MATHEMATICAL FRAKTUR SMALL I
    {"Igr",              0x00399},  // GREEK CAPITAL LETTER IOTA
    {"igr",              0x003B9},  // GREEK SMALL LETTER IOTA
    {"Igrave",           0x000CC},  // LATIN CAPITAL LETTER I WITH GRAVE
    {"igrave",           0x000EC},  // LATIN SMALL LETTER I WITH GRAVE
    {"ii",               0x02148},  // DOUBLE-STRUCK ITALIC SMALL I
    {"iiiint",           0x02A0C},  // QUADRUPLE INTEGRAL OPERATOR
    {"iiint",            0x0222D},  // TRIPLE INTEGRAL
    {"iinfin",           0x029DC},  // INCOMPLETE INFINITY
    {"iiota",            0x02129},  // TURNED GREEK SMALL LETTER IOTA
    {"IJlig",            0x00132},  // LATIN CAPITAL LIGATURE IJ
    {"ijlig",            0x00133},  // LATIN SMALL LIGATURE IJ
    {"Im",               0x02111},  // BLACK-LETTER CAPITAL I
    {"Imacr",            0x0012A},  // LATIN CAPITAL LETTER I WITH MACRON
    {"imacr",            0x0012B},  // LATIN SMALL LETTER I WITH MACRON
    {"image",            0x02111},  // BLACK-LETTER CAPITAL I
    {"ImaginaryI",       0x02148},  // DOUBLE-STRUCK ITALIC SMALL I
    {"imagline",         0x02110},  // SCRIPT CAPITAL I
    {"imagpart",         0x02111},  // BLACK-LETTER CAPITAL I
    {"imath",            0x00131},  // LATIN SMALL LETTER DOTLESS I
    {"imof",             0x022B7},  // IMAGE OF
    {"imped",            0x001B5},  // LATIN CAPITAL LETTER Z WITH STROKE
    {"Implies",          0x021D2},  // RIGHTWARDS DOUBLE ARROW
    {"in",               0x02208},  // ELEMENT OF
    {"incare",           0x02105},  // CARE OF
    {"infin",            0x0221E},  // INFINITY
    {"infintie",         0x029DD},  // TIE OVER INFINITY
    {"inodot",           0x00131},  // LATIN SMALL LETTER DOTLESS I
    {"int",              0x0222B},  // INTEGRAL
    {"Int",              0x0222C},  // DOUBLE INTEGRAL
    {"intcal",           0x022BA},  // INTERCALATE
    {"integers",         0x02124},  // DOUBLE-STRUCK CAPITAL Z
    {"Integral",         0x0222B},  // INTEGRAL
    {"intercal",         0x022BA},  // INTERCALATE
    {"Intersection",     0x022C2},  // N-ARY INTERSECTION
    {"intlarhk",         0x02A17},  // INTEGRAL WITH LEFTWARDS ARROW WITH HOOK
    {"intprod",          0x02A3C},  // INTERIOR PRODUCT
    {"InvisibleComma",   0x02063},  // INVISIBLE SEPARATOR
    {"InvisibleTimes",   0x02062},  // INVISIBLE TIMES
    {"IOcy",             0x00401},  // CYRILLIC CAPITAL LETTER IO
    {"iocy",             0x00451},  // CYRILLIC SMALL LETTER IO
    {"Iogon",            0x0012E},  // LATIN CAPITAL LETTER I WITH OGONEK
    {"iogon",            0x0012F},  // LATIN SMALL LETTER I WITH OGONEK
    {"Iopf",             0x1D540},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL I
    {"iopf",             0x1D55A},  // MATHEMATICAL DOUBLE-STRUCK SMALL I
    {"Iota",             0x00399},  // GREEK CAPITAL LETTER IOTA
    {"iota",             0x003B9},  // GREEK SMALL LETTER IOTA
    {"iprod",            0x02A3C},  // INTERIOR PRODUCT
    {"iquest",           0x000BF},  // INVERTED QUESTION MARK
    {"Iscr",             0x02110},  // SCRIPT CAPITAL I
    {"iscr",             0x1D4BE},  // MATHEMATICAL SCRIPT SMALL I
    {"isin",             0x02208},  // ELEMENT OF
    {"isindot",          0x022F5},  // ELEMENT OF WITH DOT ABOVE
    {"isinE",            0x022F9},  // ELEMENT OF WITH TWO HORIZONTAL STROKES
    {"isins",            0x022F4},  // SMALL ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
    {"isinsv",           0x022F3},  // ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
    {"isinv",            0x02208},  // ELEMENT OF
    {"it",               0x02062},  // INVISIBLE TIMES
    {"Itilde",           0x00128},  // LATIN CAPITAL LETTER I WITH TILDE
    {"itilde",           0x00129},  // LATIN SMALL LETTER I WITH TILDE
    {"Iukcy",            0x00406},  // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    {"iukcy",            0x00456},  // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    {"Iuml",             0x000CF},  // LATIN CAPITAL LETTER I WITH DIAERESIS
    {"iuml",             0x000EF},  // LATIN SMALL LETTER I WITH DIAERESIS
];

immutable NameId[] namesJ =
[
    {"Jcirc",            0x00134},  // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    {"jcirc",            0x00135},  // LATIN SMALL LETTER J WITH CIRCUMFLEX
    {"Jcy",              0x00419},  // CYRILLIC CAPITAL LETTER SHORT I
    {"jcy",              0x00439},  // CYRILLIC SMALL LETTER SHORT I
    {"Jfr",              0x1D50D},  // MATHEMATICAL FRAKTUR CAPITAL J
    {"jfr",              0x1D527},  // MATHEMATICAL FRAKTUR SMALL J
    {"jmath",            0x00237},  // LATIN SMALL LETTER DOTLESS J
    {"Jopf",             0x1D541},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL J
    {"jopf",             0x1D55B},  // MATHEMATICAL DOUBLE-STRUCK SMALL J
    {"Jscr",             0x1D4A5},  // MATHEMATICAL SCRIPT CAPITAL J
    {"jscr",             0x1D4BF},  // MATHEMATICAL SCRIPT SMALL J
    {"Jsercy",           0x00408},  // CYRILLIC CAPITAL LETTER JE
    {"jsercy",           0x00458},  // CYRILLIC SMALL LETTER JE
    {"Jukcy",            0x00404},  // CYRILLIC CAPITAL LETTER UKRAINIAN IE
    {"jukcy",            0x00454},  // CYRILLIC SMALL LETTER UKRAINIAN IE
];

immutable NameId[] namesK =
[
    {"Kappa",            0x0039A},  // GREEK CAPITAL LETTER KAPPA
    {"kappa",            0x003BA},  // GREEK SMALL LETTER KAPPA
    {"kappav",           0x003F0},  // GREEK KAPPA SYMBOL
    {"Kcedil",           0x00136},  // LATIN CAPITAL LETTER K WITH CEDILLA
    {"kcedil",           0x00137},  // LATIN SMALL LETTER K WITH CEDILLA
    {"Kcy",              0x0041A},  // CYRILLIC CAPITAL LETTER KA
    {"kcy",              0x0043A},  // CYRILLIC SMALL LETTER KA
    {"Kfr",              0x1D50E},  // MATHEMATICAL FRAKTUR CAPITAL K
    {"kfr",              0x1D528},  // MATHEMATICAL FRAKTUR SMALL K
    {"Kgr",              0x0039A},  // GREEK CAPITAL LETTER KAPPA
    {"kgr",              0x003BA},  // GREEK SMALL LETTER KAPPA
    {"kgreen",           0x00138},  // LATIN SMALL LETTER KRA
    {"KHcy",             0x00425},  // CYRILLIC CAPITAL LETTER HA
    {"khcy",             0x00445},  // CYRILLIC SMALL LETTER HA
    {"KHgr",             0x003A7},  // GREEK CAPITAL LETTER CHI
    {"khgr",             0x003C7},  // GREEK SMALL LETTER CHI
    {"KJcy",             0x0040C},  // CYRILLIC CAPITAL LETTER KJE
    {"kjcy",             0x0045C},  // CYRILLIC SMALL LETTER KJE
    {"Kopf",             0x1D542},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL K
    {"kopf",             0x1D55C},  // MATHEMATICAL DOUBLE-STRUCK SMALL K
    {"Kscr",             0x1D4A6},  // MATHEMATICAL SCRIPT CAPITAL K
    {"kscr",             0x1D4C0},  // MATHEMATICAL SCRIPT SMALL K
];

immutable NameId[] namesL =
[
    {"lAarr",            0x021DA},  // LEFTWARDS TRIPLE ARROW
    {"Lacute",           0x00139},  // LATIN CAPITAL LETTER L WITH ACUTE
    {"lacute",           0x0013A},  // LATIN SMALL LETTER L WITH ACUTE
    {"laemptyv",         0x029B4},  // EMPTY SET WITH LEFT ARROW ABOVE
    {"lagran",           0x02112},  // SCRIPT CAPITAL L
    {"Lambda",           0x0039B},  // GREEK CAPITAL LETTER LAMDA
    {"lambda",           0x003BB},  // GREEK SMALL LETTER LAMDA
    {"lang",             0x027E8},  // MATHEMATICAL LEFT ANGLE BRACKET
    {"Lang",             0x027EA},  // MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
    {"langd",            0x02991},  // LEFT ANGLE BRACKET WITH DOT
    {"langle",           0x027E8},  // MATHEMATICAL LEFT ANGLE BRACKET
    {"lap",              0x02A85},  // LESS-THAN OR APPROXIMATE
    {"Laplacetrf",       0x02112},  // SCRIPT CAPITAL L
    {"laquo",            0x000AB},  // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    {"larr",             0x02190},  // LEFTWARDS ARROW
    {"Larr",             0x0219E},  // LEFTWARDS TWO HEADED ARROW
    {"lArr",             0x021D0},  // LEFTWARDS DOUBLE ARROW
    {"larrb",            0x021E4},  // LEFTWARDS ARROW TO BAR
    {"larrbfs",          0x0291F},  // LEFTWARDS ARROW FROM BAR TO BLACK DIAMOND
    {"larrfs",           0x0291D},  // LEFTWARDS ARROW TO BLACK DIAMOND
    {"larrhk",           0x021A9},  // LEFTWARDS ARROW WITH HOOK
    {"larrlp",           0x021AB},  // LEFTWARDS ARROW WITH LOOP
    {"larrpl",           0x02939},  // LEFT-SIDE ARC ANTICLOCKWISE ARROW
    {"larrsim",          0x02973},  // LEFTWARDS ARROW ABOVE TILDE OPERATOR
    {"larrtl",           0x021A2},  // LEFTWARDS ARROW WITH TAIL
    {"lat",              0x02AAB},  // LARGER THAN
    {"latail",           0x02919},  // LEFTWARDS ARROW-TAIL
    {"lAtail",           0x0291B},  // LEFTWARDS DOUBLE ARROW-TAIL
    {"late",             0x02AAD},  // LARGER THAN OR EQUAL TO
    {"lates",            0x02AAD, 0x0FE00},  // LARGER THAN OR slanted EQUAL
    {"lbarr",            0x0290C},  // LEFTWARDS DOUBLE DASH ARROW
    {"lBarr",            0x0290E},  // LEFTWARDS TRIPLE DASH ARROW
    {"lbbrk",            0x02772},  // LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
    {"lbrace",           0x0007B},  // LEFT CURLY BRACKET
    {"lbrack",           0x0005B},  // LEFT SQUARE BRACKET
    {"lbrke",            0x0298B},  // LEFT SQUARE BRACKET WITH UNDERBAR
    {"lbrksld",          0x0298F},  // LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
    {"lbrkslu",          0x0298D},  // LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
    {"Lcaron",           0x0013D},  // LATIN CAPITAL LETTER L WITH CARON
    {"lcaron",           0x0013E},  // LATIN SMALL LETTER L WITH CARON
    {"Lcedil",           0x0013B},  // LATIN CAPITAL LETTER L WITH CEDILLA
    {"lcedil",           0x0013C},  // LATIN SMALL LETTER L WITH CEDILLA
    {"lceil",            0x02308},  // LEFT CEILING
    {"lcub",             0x0007B},  // LEFT CURLY BRACKET
    {"Lcy",              0x0041B},  // CYRILLIC CAPITAL LETTER EL
    {"lcy",              0x0043B},  // CYRILLIC SMALL LETTER EL
    {"ldca",             0x02936},  // ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS
    {"ldquo",            0x0201C},  // LEFT DOUBLE QUOTATION MARK
    {"ldquor",           0x0201E},  // DOUBLE LOW-9 QUOTATION MARK
    {"ldrdhar",          0x02967},  // LEFTWARDS HARPOON WITH BARB DOWN ABOVE RIGHTWARDS HARPOON WITH BARB DOWN
    {"ldrushar",         0x0294B},  // LEFT BARB DOWN RIGHT BARB UP HARPOON
    {"ldsh",             0x021B2},  // DOWNWARDS ARROW WITH TIP LEFTWARDS
    {"le",               0x02264},  // LESS-THAN OR EQUAL TO
    {"lE",               0x02266},  // LESS-THAN OVER EQUAL TO
    {"LeftAngleBracket", 0x027E8},  // MATHEMATICAL LEFT ANGLE BRACKET
    {"leftarrow",        0x02190},  // LEFTWARDS ARROW
    {"LeftArrow",        0x02190},  // LEFTWARDS ARROW
    {"Leftarrow",        0x021D0},  // LEFTWARDS DOUBLE ARROW
    {"LeftArrowBar",     0x021E4},  // LEFTWARDS ARROW TO BAR
    {"LeftArrowRightArrow", 0x021C6},  // LEFTWARDS ARROW OVER RIGHTWARDS ARROW
    {"leftarrowtail",    0x021A2},  // LEFTWARDS ARROW WITH TAIL
    {"LeftCeiling",      0x02308},  // LEFT CEILING
    {"LeftDoubleBracket", 0x027E6},  // MATHEMATICAL LEFT WHITE SQUARE BRACKET
    {"LeftDownTeeVector", 0x02961},  // DOWNWARDS HARPOON WITH BARB LEFT FROM BAR
    {"LeftDownVector",   0x021C3},  // DOWNWARDS HARPOON WITH BARB LEFTWARDS
    {"LeftDownVectorBar", 0x02959},  // DOWNWARDS HARPOON WITH BARB LEFT TO BAR
    {"LeftFloor",        0x0230A},  // LEFT FLOOR
    {"leftharpoondown",  0x021BD},  // LEFTWARDS HARPOON WITH BARB DOWNWARDS
    {"leftharpoonup",    0x021BC},  // LEFTWARDS HARPOON WITH BARB UPWARDS
    {"leftleftarrows",   0x021C7},  // LEFTWARDS PAIRED ARROWS
    {"leftrightarrow",   0x02194},  // LEFT RIGHT ARROW
    {"LeftRightArrow",   0x02194},  // LEFT RIGHT ARROW
    {"Leftrightarrow",   0x021D4},  // LEFT RIGHT DOUBLE ARROW
    {"leftrightarrows",  0x021C6},  // LEFTWARDS ARROW OVER RIGHTWARDS ARROW
    {"leftrightharpoons", 0x021CB},  // LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
    {"leftrightsquigarrow", 0x021AD},  // LEFT RIGHT WAVE ARROW
    {"LeftRightVector",  0x0294E},  // LEFT BARB UP RIGHT BARB UP HARPOON
    {"LeftTee",          0x022A3},  // LEFT TACK
    {"LeftTeeArrow",     0x021A4},  // LEFTWARDS ARROW FROM BAR
    {"LeftTeeVector",    0x0295A},  // LEFTWARDS HARPOON WITH BARB UP FROM BAR
    {"leftthreetimes",   0x022CB},  // LEFT SEMIDIRECT PRODUCT
    {"LeftTriangle",     0x022B2},  // NORMAL SUBGROUP OF
    {"LeftTriangleBar",  0x029CF},  // LEFT TRIANGLE BESIDE VERTICAL BAR
    {"LeftTriangleEqual", 0x022B4},  // NORMAL SUBGROUP OF OR EQUAL TO
    {"LeftUpDownVector", 0x02951},  // UP BARB LEFT DOWN BARB LEFT HARPOON
    {"LeftUpTeeVector",  0x02960},  // UPWARDS HARPOON WITH BARB LEFT FROM BAR
    {"LeftUpVector",     0x021BF},  // UPWARDS HARPOON WITH BARB LEFTWARDS
    {"LeftUpVectorBar",  0x02958},  // UPWARDS HARPOON WITH BARB LEFT TO BAR
    {"LeftVector",       0x021BC},  // LEFTWARDS HARPOON WITH BARB UPWARDS
    {"LeftVectorBar",    0x02952},  // LEFTWARDS HARPOON WITH BARB UP TO BAR
    {"leg",              0x022DA},  // LESS-THAN EQUAL TO OR GREATER-THAN
    {"lEg",              0x02A8B},  // LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN
    {"leq",              0x02264},  // LESS-THAN OR EQUAL TO
    {"leqq",             0x02266},  // LESS-THAN OVER EQUAL TO
    {"leqslant",         0x02A7D},  // LESS-THAN OR SLANTED EQUAL TO
    {"les",              0x02A7D},  // LESS-THAN OR SLANTED EQUAL TO
    {"lescc",            0x02AA8},  // LESS-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL
    {"lesdot",           0x02A7F},  // LESS-THAN OR SLANTED EQUAL TO WITH DOT INSIDE
    {"lesdoto",          0x02A81},  // LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE
    {"lesdotor",         0x02A83},  // LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE RIGHT
    {"lesg",             0x022DA, 0x0FE00},  // LESS-THAN slanted EQUAL TO OR GREATER-THAN
    {"lesges",           0x02A93},  // LESS-THAN ABOVE SLANTED EQUAL ABOVE GREATER-THAN ABOVE SLANTED EQUAL
    {"lessapprox",       0x02A85},  // LESS-THAN OR APPROXIMATE
    {"lessdot",          0x022D6},  // LESS-THAN WITH DOT
    {"lesseqgtr",        0x022DA},  // LESS-THAN EQUAL TO OR GREATER-THAN
    {"lesseqqgtr",       0x02A8B},  // LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN
    {"LessEqualGreater", 0x022DA},  // LESS-THAN EQUAL TO OR GREATER-THAN
    {"LessFullEqual",    0x02266},  // LESS-THAN OVER EQUAL TO
    {"LessGreater",      0x02276},  // LESS-THAN OR GREATER-THAN
    {"lessgtr",          0x02276},  // LESS-THAN OR GREATER-THAN
    {"LessLess",         0x02AA1},  // DOUBLE NESTED LESS-THAN
    {"lesssim",          0x02272},  // LESS-THAN OR EQUIVALENT TO
    {"LessSlantEqual",   0x02A7D},  // LESS-THAN OR SLANTED EQUAL TO
    {"LessTilde",        0x02272},  // LESS-THAN OR EQUIVALENT TO
    {"lfisht",           0x0297C},  // LEFT FISH TAIL
    {"lfloor",           0x0230A},  // LEFT FLOOR
    {"Lfr",              0x1D50F},  // MATHEMATICAL FRAKTUR CAPITAL L
    {"lfr",              0x1D529},  // MATHEMATICAL FRAKTUR SMALL L
    {"lg",               0x02276},  // LESS-THAN OR GREATER-THAN
    {"lgE",              0x02A91},  // LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL
    {"Lgr",              0x0039B},  // GREEK CAPITAL LETTER LAMDA
    {"lgr",              0x003BB},  // GREEK SMALL LETTER LAMDA
    {"lHar",             0x02962},  // LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN
    {"lhard",            0x021BD},  // LEFTWARDS HARPOON WITH BARB DOWNWARDS
    {"lharu",            0x021BC},  // LEFTWARDS HARPOON WITH BARB UPWARDS
    {"lharul",           0x0296A},  // LEFTWARDS HARPOON WITH BARB UP ABOVE LONG DASH
    {"lhblk",            0x02584},  // LOWER HALF BLOCK
    {"LJcy",             0x00409},  // CYRILLIC CAPITAL LETTER LJE
    {"ljcy",             0x00459},  // CYRILLIC SMALL LETTER LJE
    {"ll",               0x0226A},  // MUCH LESS-THAN
    {"Ll",               0x022D8},  // VERY MUCH LESS-THAN
    {"llarr",            0x021C7},  // LEFTWARDS PAIRED ARROWS
    {"llcorner",         0x0231E},  // BOTTOM LEFT CORNER
    {"Lleftarrow",       0x021DA},  // LEFTWARDS TRIPLE ARROW
    {"llhard",           0x0296B},  // LEFTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH
    {"lltri",            0x025FA},  // LOWER LEFT TRIANGLE
    {"Lmidot",           0x0013F},  // LATIN CAPITAL LETTER L WITH MIDDLE DOT
    {"lmidot",           0x00140},  // LATIN SMALL LETTER L WITH MIDDLE DOT
    {"lmoust",           0x023B0},  // UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION
    {"lmoustache",       0x023B0},  // UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION
    {"lnap",             0x02A89},  // LESS-THAN AND NOT APPROXIMATE
    {"lnapprox",         0x02A89},  // LESS-THAN AND NOT APPROXIMATE
    {"lnE",              0x02268},  // LESS-THAN BUT NOT EQUAL TO
    {"lne",              0x02A87},  // LESS-THAN AND SINGLE-LINE NOT EQUAL TO
    {"lneq",             0x02A87},  // LESS-THAN AND SINGLE-LINE NOT EQUAL TO
    {"lneqq",            0x02268},  // LESS-THAN BUT NOT EQUAL TO
    {"lnsim",            0x022E6},  // LESS-THAN BUT NOT EQUIVALENT TO
    {"loang",            0x027EC},  // MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
    {"loarr",            0x021FD},  // LEFTWARDS OPEN-HEADED ARROW
    {"lobrk",            0x027E6},  // MATHEMATICAL LEFT WHITE SQUARE BRACKET
    {"longleftarrow",    0x027F5},  // LONG LEFTWARDS ARROW
    {"LongLeftArrow",    0x027F5},  // LONG LEFTWARDS ARROW
    {"Longleftarrow",    0x027F8},  // LONG LEFTWARDS DOUBLE ARROW
    {"longleftrightarrow", 0x027F7},  // LONG LEFT RIGHT ARROW
    {"LongLeftRightArrow", 0x027F7},  // LONG LEFT RIGHT ARROW
    {"Longleftrightarrow", 0x027FA},  // LONG LEFT RIGHT DOUBLE ARROW
    {"longmapsto",       0x027FC},  // LONG RIGHTWARDS ARROW FROM BAR
    {"longrightarrow",   0x027F6},  // LONG RIGHTWARDS ARROW
    {"LongRightArrow",   0x027F6},  // LONG RIGHTWARDS ARROW
    {"Longrightarrow",   0x027F9},  // LONG RIGHTWARDS DOUBLE ARROW
    {"looparrowleft",    0x021AB},  // LEFTWARDS ARROW WITH LOOP
    {"looparrowright",   0x021AC},  // RIGHTWARDS ARROW WITH LOOP
    {"lopar",            0x02985},  // LEFT WHITE PARENTHESIS
    {"Lopf",             0x1D543},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL L
    {"lopf",             0x1D55D},  // MATHEMATICAL DOUBLE-STRUCK SMALL L
    {"loplus",           0x02A2D},  // PLUS SIGN IN LEFT HALF CIRCLE
    {"lotimes",          0x02A34},  // MULTIPLICATION SIGN IN LEFT HALF CIRCLE
    {"lowast",           0x02217},  // ASTERISK OPERATOR
    {"lowbar",           0x0005F},  // LOW LINE
    {"LowerLeftArrow",   0x02199},  // SOUTH WEST ARROW
    {"LowerRightArrow",  0x02198},  // SOUTH EAST ARROW
    {"loz",              0x025CA},  // LOZENGE
    {"lozenge",          0x025CA},  // LOZENGE
    {"lozf",             0x029EB},  // BLACK LOZENGE
    {"lpar",             0x00028},  // LEFT PARENTHESIS
    {"lparlt",           0x02993},  // LEFT ARC LESS-THAN BRACKET
    {"lrarr",            0x021C6},  // LEFTWARDS ARROW OVER RIGHTWARDS ARROW
    {"lrcorner",         0x0231F},  // BOTTOM RIGHT CORNER
    {"lrhar",            0x021CB},  // LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
    {"lrhard",           0x0296D},  // RIGHTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH
    {"lrm",              0x0200E},  // LEFT-TO-RIGHT MARK
    {"lrtri",            0x022BF},  // RIGHT TRIANGLE
    {"lsaquo",           0x02039},  // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    {"Lscr",             0x02112},  // SCRIPT CAPITAL L
    {"lscr",             0x1D4C1},  // MATHEMATICAL SCRIPT SMALL L
    {"lsh",              0x021B0},  // UPWARDS ARROW WITH TIP LEFTWARDS
    {"Lsh",              0x021B0},  // UPWARDS ARROW WITH TIP LEFTWARDS
    {"lsim",             0x02272},  // LESS-THAN OR EQUIVALENT TO
    {"lsime",            0x02A8D},  // LESS-THAN ABOVE SIMILAR OR EQUAL
    {"lsimg",            0x02A8F},  // LESS-THAN ABOVE SIMILAR ABOVE GREATER-THAN
    {"lsqb",             0x0005B},  // LEFT SQUARE BRACKET
    {"lsquo",            0x02018},  // LEFT SINGLE QUOTATION MARK
    {"lsquor",           0x0201A},  // SINGLE LOW-9 QUOTATION MARK
    {"Lstrok",           0x00141},  // LATIN CAPITAL LETTER L WITH STROKE
    {"lstrok",           0x00142},  // LATIN SMALL LETTER L WITH STROKE
    {"lt",               0x0003C},  // LESS-THAN SIGN
    {"LT",               0x0003C},  // LESS-THAN SIGN
    {"Lt",               0x0226A},  // MUCH LESS-THAN
    {"ltcc",             0x02AA6},  // LESS-THAN CLOSED BY CURVE
    {"ltcir",            0x02A79},  // LESS-THAN WITH CIRCLE INSIDE
    {"ltdot",            0x022D6},  // LESS-THAN WITH DOT
    {"lthree",           0x022CB},  // LEFT SEMIDIRECT PRODUCT
    {"ltimes",           0x022C9},  // LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
    {"ltlarr",           0x02976},  // LESS-THAN ABOVE LEFTWARDS ARROW
    {"ltquest",          0x02A7B},  // LESS-THAN WITH QUESTION MARK ABOVE
    {"ltri",             0x025C3},  // WHITE LEFT-POINTING SMALL TRIANGLE
    {"ltrie",            0x022B4},  // NORMAL SUBGROUP OF OR EQUAL TO
    {"ltrif",            0x025C2},  // BLACK LEFT-POINTING SMALL TRIANGLE
    {"ltrPar",           0x02996},  // DOUBLE RIGHT ARC LESS-THAN BRACKET
    {"lurdshar",         0x0294A},  // LEFT BARB UP RIGHT BARB DOWN HARPOON
    {"luruhar",          0x02966},  // LEFTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB UP
    {"lvertneqq",        0x02268, 0x0FE00},  // LESS-THAN BUT NOT EQUAL TO - with vertical stroke
    {"lvnE",             0x02268, 0x0FE00},  // LESS-THAN BUT NOT EQUAL TO - with vertical stroke
];

immutable NameId[] namesM =
[
    {"macr",             0x000AF},  // MACRON
    {"male",             0x02642},  // MALE SIGN
    {"malt",             0x02720},  // MALTESE CROSS
    {"maltese",          0x02720},  // MALTESE CROSS
    {"map",              0x021A6},  // RIGHTWARDS ARROW FROM BAR
    {"Map",              0x02905},  // RIGHTWARDS TWO-HEADED ARROW FROM BAR
    {"mapsto",           0x021A6},  // RIGHTWARDS ARROW FROM BAR
    {"mapstodown",       0x021A7},  // DOWNWARDS ARROW FROM BAR
    {"mapstoleft",       0x021A4},  // LEFTWARDS ARROW FROM BAR
    {"mapstoup",         0x021A5},  // UPWARDS ARROW FROM BAR
    {"marker",           0x025AE},  // BLACK VERTICAL RECTANGLE
    {"mcomma",           0x02A29},  // MINUS SIGN WITH COMMA ABOVE
    {"Mcy",              0x0041C},  // CYRILLIC CAPITAL LETTER EM
    {"mcy",              0x0043C},  // CYRILLIC SMALL LETTER EM
    {"mdash",            0x02014},  // EM DASH
    {"mDDot",            0x0223A},  // GEOMETRIC PROPORTION
    {"measuredangle",    0x02221},  // MEASURED ANGLE
    {"MediumSpace",      0x0205F},  // MEDIUM MATHEMATICAL SPACE
    {"Mellintrf",        0x02133},  // SCRIPT CAPITAL M
    {"Mfr",              0x1D510},  // MATHEMATICAL FRAKTUR CAPITAL M
    {"mfr",              0x1D52A},  // MATHEMATICAL FRAKTUR SMALL M
    {"Mgr",              0x0039C},  // GREEK CAPITAL LETTER MU
    {"mgr",              0x003BC},  // GREEK SMALL LETTER MU
    {"mho",              0x02127},  // INVERTED OHM SIGN
    {"micro",            0x000B5},  // MICRO SIGN
    {"mid",              0x02223},  // DIVIDES
    {"midast",           0x0002A},  // ASTERISK
    {"midcir",           0x02AF0},  // VERTICAL LINE WITH CIRCLE BELOW
    {"middot",           0x000B7},  // MIDDLE DOT
    {"minus",            0x02212},  // MINUS SIGN
    {"minusb",           0x0229F},  // SQUARED MINUS
    {"minusd",           0x02238},  // DOT MINUS
    {"minusdu",          0x02A2A},  // MINUS SIGN WITH DOT BELOW
    {"MinusPlus",        0x02213},  // MINUS-OR-PLUS SIGN
    {"mlcp",             0x02ADB},  // TRANSVERSAL INTERSECTION
    {"mldr",             0x02026},  // HORIZONTAL ELLIPSIS
    {"mnplus",           0x02213},  // MINUS-OR-PLUS SIGN
    {"models",           0x022A7},  // MODELS
    {"Mopf",             0x1D544},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL M
    {"mopf",             0x1D55E},  // MATHEMATICAL DOUBLE-STRUCK SMALL M
    {"mp",               0x02213},  // MINUS-OR-PLUS SIGN
    {"Mscr",             0x02133},  // SCRIPT CAPITAL M
    {"mscr",             0x1D4C2},  // MATHEMATICAL SCRIPT SMALL M
    {"mstpos",           0x0223E},  // INVERTED LAZY S
    {"Mu",               0x0039C},  // GREEK CAPITAL LETTER MU
    {"mu",               0x003BC},  // GREEK SMALL LETTER MU
    {"multimap",         0x022B8},  // MULTIMAP
    {"mumap",            0x022B8},  // MULTIMAP
];

immutable NameId[] namesN =
[
    {"nabla",            0x02207},  // NABLA
    {"Nacute",           0x00143},  // LATIN CAPITAL LETTER N WITH ACUTE
    {"nacute",           0x00144},  // LATIN SMALL LETTER N WITH ACUTE
    {"nang",             0x02220, 0x020D2},  // ANGLE with vertical line
    {"nap",              0x02249},  // NOT ALMOST EQUAL TO
    {"napE",             0x02A70, 0x00338},  // APPROXIMATELY EQUAL OR EQUAL TO with slash
    {"napid",            0x0224B, 0x00338},  // TRIPLE TILDE with slash
    {"napos",            0x00149},  // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    {"napprox",          0x02249},  // NOT ALMOST EQUAL TO
    {"natur",            0x0266E},  // MUSIC NATURAL SIGN
    {"natural",          0x0266E},  // MUSIC NATURAL SIGN
    {"naturals",         0x02115},  // DOUBLE-STRUCK CAPITAL N
    {"nbsp",             0x000A0},  // NO-BREAK SPACE
    {"nbump",            0x0224E, 0x00338},  // GEOMETRICALLY EQUIVALENT TO with slash
    {"nbumpe",           0x0224F, 0x00338},  // DIFFERENCE BETWEEN with slash
    {"ncap",             0x02A43},  // INTERSECTION WITH OVERBAR
    {"Ncaron",           0x00147},  // LATIN CAPITAL LETTER N WITH CARON
    {"ncaron",           0x00148},  // LATIN SMALL LETTER N WITH CARON
    {"Ncedil",           0x00145},  // LATIN CAPITAL LETTER N WITH CEDILLA
    {"ncedil",           0x00146},  // LATIN SMALL LETTER N WITH CEDILLA
    {"ncong",            0x02247},  // NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
    {"ncongdot",         0x02A6D, 0x00338},  // CONGRUENT WITH DOT ABOVE with slash
    {"ncup",             0x02A42},  // UNION WITH OVERBAR
    {"Ncy",              0x0041D},  // CYRILLIC CAPITAL LETTER EN
    {"ncy",              0x0043D},  // CYRILLIC SMALL LETTER EN
    {"ndash",            0x02013},  // EN DASH
    {"ne",               0x02260},  // NOT EQUAL TO
    {"nearhk",           0x02924},  // NORTH EAST ARROW WITH HOOK
    {"nearr",            0x02197},  // NORTH EAST ARROW
    {"neArr",            0x021D7},  // NORTH EAST DOUBLE ARROW
    {"nearrow",          0x02197},  // NORTH EAST ARROW
    {"nedot",            0x02250, 0x00338},  // APPROACHES THE LIMIT with slash
    {"NegativeMediumSpace", 0x0200B},  // ZERO WIDTH SPACE
    {"NegativeThickSpace", 0x0200B},  // ZERO WIDTH SPACE
    {"NegativeThinSpace", 0x0200B},  // ZERO WIDTH SPACE
    {"NegativeVeryThinSpace", 0x0200B},  // ZERO WIDTH SPACE
    {"nequiv",           0x02262},  // NOT IDENTICAL TO
    {"nesear",           0x02928},  // NORTH EAST ARROW AND SOUTH EAST ARROW
    {"nesim",            0x02242, 0x00338},  // MINUS TILDE with slash
    {"NestedGreaterGreater", 0x0226B},  // MUCH GREATER-THAN
    {"NestedLessLess",   0x0226A},  // MUCH LESS-THAN
    {"NewLine",          0x0000A},  // LINE FEED (LF)
    {"nexist",           0x02204},  // THERE DOES NOT EXIST
    {"nexists",          0x02204},  // THERE DOES NOT EXIST
    {"Nfr",              0x1D511},  // MATHEMATICAL FRAKTUR CAPITAL N
    {"nfr",              0x1D52B},  // MATHEMATICAL FRAKTUR SMALL N
    {"ngE",              0x02267, 0x00338},  // GREATER-THAN OVER EQUAL TO with slash
    {"nge",              0x02271},  // NEITHER GREATER-THAN NOR EQUAL TO
    {"ngeq",             0x02271},  // NEITHER GREATER-THAN NOR EQUAL TO
    {"ngeqq",            0x02267, 0x00338},  // GREATER-THAN OVER EQUAL TO with slash
    {"ngeqslant",        0x02A7E, 0x00338},  // GREATER-THAN OR SLANTED EQUAL TO with slash
    {"nges",             0x02A7E, 0x00338},  // GREATER-THAN OR SLANTED EQUAL TO with slash
    {"nGg",              0x022D9, 0x00338},  // VERY MUCH GREATER-THAN with slash
    {"Ngr",              0x0039D},  // GREEK CAPITAL LETTER NU
    {"ngr",              0x003BD},  // GREEK SMALL LETTER NU
    {"ngsim",            0x02275},  // NEITHER GREATER-THAN NOR EQUIVALENT TO
    {"nGt",              0x0226B, 0x020D2},  // MUCH GREATER THAN with vertical line
    {"ngt",              0x0226F},  // NOT GREATER-THAN
    {"ngtr",             0x0226F},  // NOT GREATER-THAN
    {"nGtv",             0x0226B, 0x00338},  // MUCH GREATER THAN with slash
    {"nharr",            0x021AE},  // LEFT RIGHT ARROW WITH STROKE
    {"nhArr",            0x021CE},  // LEFT RIGHT DOUBLE ARROW WITH STROKE
    {"nhpar",            0x02AF2},  // PARALLEL WITH HORIZONTAL STROKE
    {"ni",               0x0220B},  // CONTAINS AS MEMBER
    {"nis",              0x022FC},  // SMALL CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
    {"nisd",             0x022FA},  // CONTAINS WITH LONG HORIZONTAL STROKE
    {"niv",              0x0220B},  // CONTAINS AS MEMBER
    {"NJcy",             0x0040A},  // CYRILLIC CAPITAL LETTER NJE
    {"njcy",             0x0045A},  // CYRILLIC SMALL LETTER NJE
    {"nlarr",            0x0219A},  // LEFTWARDS ARROW WITH STROKE
    {"nlArr",            0x021CD},  // LEFTWARDS DOUBLE ARROW WITH STROKE
    {"nldr",             0x02025},  // TWO DOT LEADER
    {"nlE",              0x02266, 0x00338},  // LESS-THAN OVER EQUAL TO with slash
    {"nle",              0x02270},  // NEITHER LESS-THAN NOR EQUAL TO
    {"nleftarrow",       0x0219A},  // LEFTWARDS ARROW WITH STROKE
    {"nLeftarrow",       0x021CD},  // LEFTWARDS DOUBLE ARROW WITH STROKE
    {"nleftrightarrow",  0x021AE},  // LEFT RIGHT ARROW WITH STROKE
    {"nLeftrightarrow",  0x021CE},  // LEFT RIGHT DOUBLE ARROW WITH STROKE
    {"nleq",             0x02270},  // NEITHER LESS-THAN NOR EQUAL TO
    {"nleqq",            0x02266, 0x00338},  // LESS-THAN OVER EQUAL TO with slash
    {"nleqslant",        0x02A7D, 0x00338},  // LESS-THAN OR SLANTED EQUAL TO with slash
    {"nles",             0x02A7D, 0x00338},  // LESS-THAN OR SLANTED EQUAL TO with slash
    {"nless",            0x0226E},  // NOT LESS-THAN
    {"nLl",              0x022D8, 0x00338},  // VERY MUCH LESS-THAN with slash
    {"nlsim",            0x02274},  // NEITHER LESS-THAN NOR EQUIVALENT TO
    {"nLt",              0x0226A, 0x020D2},  // MUCH LESS THAN with vertical line
    {"nlt",              0x0226E},  // NOT LESS-THAN
    {"nltri",            0x022EA},  // NOT NORMAL SUBGROUP OF
    {"nltrie",           0x022EC},  // NOT NORMAL SUBGROUP OF OR EQUAL TO
    {"nLtv",             0x0226A, 0x00338},  // MUCH LESS THAN with slash
    {"nmid",             0x02224},  // DOES NOT DIVIDE
    {"NoBreak",          0x02060},  // WORD JOINER
    {"NonBreakingSpace", 0x000A0},  // NO-BREAK SPACE
    {"Nopf",             0x02115},  // DOUBLE-STRUCK CAPITAL N
    {"nopf",             0x1D55F},  // MATHEMATICAL DOUBLE-STRUCK SMALL N
    {"not",              0x000AC},  // NOT SIGN
    {"Not",              0x02AEC},  // DOUBLE STROKE NOT SIGN
    {"NotCongruent",     0x02262},  // NOT IDENTICAL TO
    {"NotCupCap",        0x0226D},  // NOT EQUIVALENT TO
    {"NotDoubleVerticalBar", 0x02226},  // NOT PARALLEL TO
    {"NotElement",       0x02209},  // NOT AN ELEMENT OF
    {"NotEqual",         0x02260},  // NOT EQUAL TO
    {"NotEqualTilde",    0x02242, 0x00338},  // MINUS TILDE with slash
    {"NotExists",        0x02204},  // THERE DOES NOT EXIST
    {"NotGreater",       0x0226F},  // NOT GREATER-THAN
    {"NotGreaterEqual",  0x02271},  // NEITHER GREATER-THAN NOR EQUAL TO
    {"NotGreaterFullEqual", 0x02267, 0x00338},  // GREATER-THAN OVER EQUAL TO with slash
    {"NotGreaterGreater", 0x0226B, 0x00338},  // MUCH GREATER THAN with slash
    {"NotGreaterLess",   0x02279},  // NEITHER GREATER-THAN NOR LESS-THAN
    {"NotGreaterSlantEqual", 0x02A7E, 0x00338},  // GREATER-THAN OR SLANTED EQUAL TO with slash
    {"NotGreaterTilde",  0x02275},  // NEITHER GREATER-THAN NOR EQUIVALENT TO
    {"NotHumpDownHump",  0x0224E, 0x00338},  // GEOMETRICALLY EQUIVALENT TO with slash
    {"NotHumpEqual",     0x0224F, 0x00338},  // DIFFERENCE BETWEEN with slash
    {"notin",            0x02209},  // NOT AN ELEMENT OF
    {"notindot",         0x022F5, 0x00338},  // ELEMENT OF WITH DOT ABOVE with slash
    {"notinE",           0x022F9, 0x00338},  // ELEMENT OF WITH TWO HORIZONTAL STROKES with slash
    {"notinva",          0x02209},  // NOT AN ELEMENT OF
    {"notinvb",          0x022F7},  // SMALL ELEMENT OF WITH OVERBAR
    {"notinvc",          0x022F6},  // ELEMENT OF WITH OVERBAR
    {"NotLeftTriangle",  0x022EA},  // NOT NORMAL SUBGROUP OF
    {"NotLeftTriangleBar", 0x029CF, 0x00338},  // LEFT TRIANGLE BESIDE VERTICAL BAR with slash
    {"NotLeftTriangleEqual", 0x022EC},  // NOT NORMAL SUBGROUP OF OR EQUAL TO
    {"NotLess",          0x0226E},  // NOT LESS-THAN
    {"NotLessEqual",     0x02270},  // NEITHER LESS-THAN NOR EQUAL TO
    {"NotLessGreater",   0x02278},  // NEITHER LESS-THAN NOR GREATER-THAN
    {"NotLessLess",      0x0226A, 0x00338},  // MUCH LESS THAN with slash
    {"NotLessSlantEqual", 0x02A7D, 0x00338},  // LESS-THAN OR SLANTED EQUAL TO with slash
    {"NotLessTilde",     0x02274},  // NEITHER LESS-THAN NOR EQUIVALENT TO
    {"NotNestedGreaterGreater", 0x02AA2, 0x00338},  // DOUBLE NESTED GREATER-THAN with slash
    {"NotNestedLessLess", 0x02AA1, 0x00338},  // DOUBLE NESTED LESS-THAN with slash
    {"notni",            0x0220C},  // DOES NOT CONTAIN AS MEMBER
    {"notniva",          0x0220C},  // DOES NOT CONTAIN AS MEMBER
    {"notnivb",          0x022FE},  // SMALL CONTAINS WITH OVERBAR
    {"notnivc",          0x022FD},  // CONTAINS WITH OVERBAR
    {"NotPrecedes",      0x02280},  // DOES NOT PRECEDE
    {"NotPrecedesEqual", 0x02AAF, 0x00338},  // PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash
    {"NotPrecedesSlantEqual", 0x022E0},  // DOES NOT PRECEDE OR EQUAL
    {"NotReverseElement", 0x0220C},  // DOES NOT CONTAIN AS MEMBER
    {"NotRightTriangle", 0x022EB},  // DOES NOT CONTAIN AS NORMAL SUBGROUP
    {"NotRightTriangleBar", 0x029D0, 0x00338},  // VERTICAL BAR BESIDE RIGHT TRIANGLE with slash
    {"NotRightTriangleEqual", 0x022ED},  // DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
    {"NotSquareSubset",  0x0228F, 0x00338},  // SQUARE IMAGE OF with slash
    {"NotSquareSubsetEqual", 0x022E2},  // NOT SQUARE IMAGE OF OR EQUAL TO
    {"NotSquareSuperset", 0x02290, 0x00338},  // SQUARE ORIGINAL OF with slash
    {"NotSquareSupersetEqual", 0x022E3},  // NOT SQUARE ORIGINAL OF OR EQUAL TO
    {"NotSubset",        0x02282, 0x020D2},  // SUBSET OF with vertical line
    {"NotSubsetEqual",   0x02288},  // NEITHER A SUBSET OF NOR EQUAL TO
    {"NotSucceeds",      0x02281},  // DOES NOT SUCCEED
    {"NotSucceedsEqual", 0x02AB0, 0x00338},  // SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash
    {"NotSucceedsSlantEqual", 0x022E1},  // DOES NOT SUCCEED OR EQUAL
    {"NotSucceedsTilde", 0x0227F, 0x00338},  // SUCCEEDS OR EQUIVALENT TO with slash
    {"NotSuperset",      0x02283, 0x020D2},  // SUPERSET OF with vertical line
    {"NotSupersetEqual", 0x02289},  // NEITHER A SUPERSET OF NOR EQUAL TO
    {"NotTilde",         0x02241},  // NOT TILDE
    {"NotTildeEqual",    0x02244},  // NOT ASYMPTOTICALLY EQUAL TO
    {"NotTildeFullEqual", 0x02247},  // NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
    {"NotTildeTilde",    0x02249},  // NOT ALMOST EQUAL TO
    {"NotVerticalBar",   0x02224},  // DOES NOT DIVIDE
    {"npar",             0x02226},  // NOT PARALLEL TO
    {"nparallel",        0x02226},  // NOT PARALLEL TO
    {"nparsl",           0x02AFD, 0x020E5},  // DOUBLE SOLIDUS OPERATOR with reverse slash
    {"npart",            0x02202, 0x00338},  // PARTIAL DIFFERENTIAL with slash
    {"npolint",          0x02A14},  // LINE INTEGRATION NOT INCLUDING THE POLE
    {"npr",              0x02280},  // DOES NOT PRECEDE
    {"nprcue",           0x022E0},  // DOES NOT PRECEDE OR EQUAL
    {"npre",             0x02AAF, 0x00338},  // PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash
    {"nprec",            0x02280},  // DOES NOT PRECEDE
    {"npreceq",          0x02AAF, 0x00338},  // PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash
    {"nrarr",            0x0219B},  // RIGHTWARDS ARROW WITH STROKE
    {"nrArr",            0x021CF},  // RIGHTWARDS DOUBLE ARROW WITH STROKE
    {"nrarrc",           0x02933, 0x00338},  // WAVE ARROW POINTING DIRECTLY RIGHT with slash
    {"nrarrw",           0x0219D, 0x00338},  // RIGHTWARDS WAVE ARROW with slash
    {"nrightarrow",      0x0219B},  // RIGHTWARDS ARROW WITH STROKE
    {"nRightarrow",      0x021CF},  // RIGHTWARDS DOUBLE ARROW WITH STROKE
    {"nrtri",            0x022EB},  // DOES NOT CONTAIN AS NORMAL SUBGROUP
    {"nrtrie",           0x022ED},  // DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
    {"nsc",              0x02281},  // DOES NOT SUCCEED
    {"nsccue",           0x022E1},  // DOES NOT SUCCEED OR EQUAL
    {"nsce",             0x02AB0, 0x00338},  // SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash
    {"Nscr",             0x1D4A9},  // MATHEMATICAL SCRIPT CAPITAL N
    {"nscr",             0x1D4C3},  // MATHEMATICAL SCRIPT SMALL N
    {"nshortmid",        0x02224},  // DOES NOT DIVIDE
    {"nshortparallel",   0x02226},  // NOT PARALLEL TO
    {"nsim",             0x02241},  // NOT TILDE
    {"nsime",            0x02244},  // NOT ASYMPTOTICALLY EQUAL TO
    {"nsimeq",           0x02244},  // NOT ASYMPTOTICALLY EQUAL TO
    {"nsmid",            0x02224},  // DOES NOT DIVIDE
    {"nspar",            0x02226},  // NOT PARALLEL TO
    {"nsqsube",          0x022E2},  // NOT SQUARE IMAGE OF OR EQUAL TO
    {"nsqsupe",          0x022E3},  // NOT SQUARE ORIGINAL OF OR EQUAL TO
    {"nsub",             0x02284},  // NOT A SUBSET OF
    {"nsube",            0x02288},  // NEITHER A SUBSET OF NOR EQUAL TO
    {"nsubE",            0x02AC5, 0x00338},  // SUBSET OF ABOVE EQUALS SIGN with slash
    {"nsubset",          0x02282, 0x020D2},  // SUBSET OF with vertical line
    {"nsubseteq",        0x02288},  // NEITHER A SUBSET OF NOR EQUAL TO
    {"nsubseteqq",       0x02AC5, 0x00338},  // SUBSET OF ABOVE EQUALS SIGN with slash
    {"nsucc",            0x02281},  // DOES NOT SUCCEED
    {"nsucceq",          0x02AB0, 0x00338},  // SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash
    {"nsup",             0x02285},  // NOT A SUPERSET OF
    {"nsupe",            0x02289},  // NEITHER A SUPERSET OF NOR EQUAL TO
    {"nsupE",            0x02AC6, 0x00338},  // SUPERSET OF ABOVE EQUALS SIGN with slash
    {"nsupset",          0x02283, 0x020D2},  // SUPERSET OF with vertical line
    {"nsupseteq",        0x02289},  // NEITHER A SUPERSET OF NOR EQUAL TO
    {"nsupseteqq",       0x02AC6, 0x00338},  // SUPERSET OF ABOVE EQUALS SIGN with slash
    {"ntgl",             0x02279},  // NEITHER GREATER-THAN NOR LESS-THAN
    {"Ntilde",           0x000D1},  // LATIN CAPITAL LETTER N WITH TILDE
    {"ntilde",           0x000F1},  // LATIN SMALL LETTER N WITH TILDE
    {"ntlg",             0x02278},  // NEITHER LESS-THAN NOR GREATER-THAN
    {"ntriangleleft",    0x022EA},  // NOT NORMAL SUBGROUP OF
    {"ntrianglelefteq",  0x022EC},  // NOT NORMAL SUBGROUP OF OR EQUAL TO
    {"ntriangleright",   0x022EB},  // DOES NOT CONTAIN AS NORMAL SUBGROUP
    {"ntrianglerighteq", 0x022ED},  // DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
    {"Nu",               0x0039D},  // GREEK CAPITAL LETTER NU
    {"nu",               0x003BD},  // GREEK SMALL LETTER NU
    {"num",              0x00023},  // NUMBER SIGN
    {"numero",           0x02116},  // NUMERO SIGN
    {"numsp",            0x02007},  // FIGURE SPACE
    {"nvap",             0x0224D, 0x020D2},  // EQUIVALENT TO with vertical line
    {"nvdash",           0x022AC},  // DOES NOT PROVE
    {"nvDash",           0x022AD},  // NOT TRUE
    {"nVdash",           0x022AE},  // DOES NOT FORCE
    {"nVDash",           0x022AF},  // NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
    {"nvge",             0x02265, 0x020D2},  // GREATER-THAN OR EQUAL TO with vertical line
    {"nvgt",             0x0003E, 0x020D2},  // GREATER-THAN SIGN with vertical line
    {"nvHarr",           0x02904},  // LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE
    {"nvinfin",          0x029DE},  // INFINITY NEGATED WITH VERTICAL BAR
    {"nvlArr",           0x02902},  // LEFTWARDS DOUBLE ARROW WITH VERTICAL STROKE
    {"nvle",             0x02264, 0x020D2},  // LESS-THAN OR EQUAL TO with vertical line
    {"nvlt",             0x0003C, 0x020D2},  // LESS-THAN SIGN with vertical line
    {"nvltrie",          0x022B4, 0x020D2},  // NORMAL SUBGROUP OF OR EQUAL TO with vertical line
    {"nvrArr",           0x02903},  // RIGHTWARDS DOUBLE ARROW WITH VERTICAL STROKE
    {"nvrtrie",          0x022B5, 0x020D2},  // CONTAINS AS NORMAL SUBGROUP OR EQUAL TO with vertical line
    {"nvsim",            0x0223C, 0x020D2},  // TILDE OPERATOR with vertical line
    {"nwarhk",           0x02923},  // NORTH WEST ARROW WITH HOOK
    {"nwarr",            0x02196},  // NORTH WEST ARROW
    {"nwArr",            0x021D6},  // NORTH WEST DOUBLE ARROW
    {"nwarrow",          0x02196},  // NORTH WEST ARROW
    {"nwnear",           0x02927},  // NORTH WEST ARROW AND NORTH EAST ARROW
];

immutable NameId[] namesO =
[
    {"Oacgr",            0x0038C},  // GREEK CAPITAL LETTER OMICRON WITH TONOS
    {"oacgr",            0x003CC},  // GREEK SMALL LETTER OMICRON WITH TONOS
    {"Oacute",           0x000D3},  // LATIN CAPITAL LETTER O WITH ACUTE
    {"oacute",           0x000F3},  // LATIN SMALL LETTER O WITH ACUTE
    {"oast",             0x0229B},  // CIRCLED ASTERISK OPERATOR
    {"ocir",             0x0229A},  // CIRCLED RING OPERATOR
    {"Ocirc",            0x000D4},  // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    {"ocirc",            0x000F4},  // LATIN SMALL LETTER O WITH CIRCUMFLEX
    {"Ocy",              0x0041E},  // CYRILLIC CAPITAL LETTER O
    {"ocy",              0x0043E},  // CYRILLIC SMALL LETTER O
    {"odash",            0x0229D},  // CIRCLED DASH
    {"Odblac",           0x00150},  // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    {"odblac",           0x00151},  // LATIN SMALL LETTER O WITH DOUBLE ACUTE
    {"odiv",             0x02A38},  // CIRCLED DIVISION SIGN
    {"odot",             0x02299},  // CIRCLED DOT OPERATOR
    {"odsold",           0x029BC},  // CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN
    {"OElig",            0x00152},  // LATIN CAPITAL LIGATURE OE
    {"oelig",            0x00153},  // LATIN SMALL LIGATURE OE
    {"ofcir",            0x029BF},  // CIRCLED BULLET
    {"Ofr",              0x1D512},  // MATHEMATICAL FRAKTUR CAPITAL O
    {"ofr",              0x1D52C},  // MATHEMATICAL FRAKTUR SMALL O
    {"ogon",             0x002DB},  // OGONEK
    {"Ogr",              0x0039F},  // GREEK CAPITAL LETTER OMICRON
    {"ogr",              0x003BF},  // GREEK SMALL LETTER OMICRON
    {"Ograve",           0x000D2},  // LATIN CAPITAL LETTER O WITH GRAVE
    {"ograve",           0x000F2},  // LATIN SMALL LETTER O WITH GRAVE
    {"ogt",              0x029C1},  // CIRCLED GREATER-THAN
    {"OHacgr",           0x0038F},  // GREEK CAPITAL LETTER OMEGA WITH TONOS
    {"ohacgr",           0x003CE},  // GREEK SMALL LETTER OMEGA WITH TONOS
    {"ohbar",            0x029B5},  // CIRCLE WITH HORIZONTAL BAR
    {"OHgr",             0x003A9},  // GREEK CAPITAL LETTER OMEGA
    {"ohgr",             0x003C9},  // GREEK SMALL LETTER OMEGA
    {"ohm",              0x003A9},  // GREEK CAPITAL LETTER OMEGA
    {"oint",             0x0222E},  // CONTOUR INTEGRAL
    {"olarr",            0x021BA},  // ANTICLOCKWISE OPEN CIRCLE ARROW
    {"olcir",            0x029BE},  // CIRCLED WHITE BULLET
    {"olcross",          0x029BB},  // CIRCLE WITH SUPERIMPOSED X
    {"oline",            0x0203E},  // OVERLINE
    {"olt",              0x029C0},  // CIRCLED LESS-THAN
    {"Omacr",            0x0014C},  // LATIN CAPITAL LETTER O WITH MACRON
    {"omacr",            0x0014D},  // LATIN SMALL LETTER O WITH MACRON
    {"Omega",            0x003A9},  // GREEK CAPITAL LETTER OMEGA
    {"omega",            0x003C9},  // GREEK SMALL LETTER OMEGA
    {"Omicron",          0x0039F},  // GREEK CAPITAL LETTER OMICRON
    {"omicron",          0x003BF},  // GREEK SMALL LETTER OMICRON
    {"omid",             0x029B6},  // CIRCLED VERTICAL BAR
    {"ominus",           0x02296},  // CIRCLED MINUS
    {"Oopf",             0x1D546},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL O
    {"oopf",             0x1D560},  // MATHEMATICAL DOUBLE-STRUCK SMALL O
    {"opar",             0x029B7},  // CIRCLED PARALLEL
    {"OpenCurlyDoubleQuote", 0x0201C},  // LEFT DOUBLE QUOTATION MARK
    {"OpenCurlyQuote",   0x02018},  // LEFT SINGLE QUOTATION MARK
    {"operp",            0x029B9},  // CIRCLED PERPENDICULAR
    {"oplus",            0x02295},  // CIRCLED PLUS
    {"or",               0x02228},  // LOGICAL OR
    {"Or",               0x02A54},  // DOUBLE LOGICAL OR
    {"orarr",            0x021BB},  // CLOCKWISE OPEN CIRCLE ARROW
    {"ord",              0x02A5D},  // LOGICAL OR WITH HORIZONTAL DASH
    {"order",            0x02134},  // SCRIPT SMALL O
    {"orderof",          0x02134},  // SCRIPT SMALL O
    {"ordf",             0x000AA},  // FEMININE ORDINAL INDICATOR
    {"ordm",             0x000BA},  // MASCULINE ORDINAL INDICATOR
    {"origof",           0x022B6},  // ORIGINAL OF
    {"oror",             0x02A56},  // TWO INTERSECTING LOGICAL OR
    {"orslope",          0x02A57},  // SLOPING LARGE OR
    {"orv",              0x02A5B},  // LOGICAL OR WITH MIDDLE STEM
    {"oS",               0x024C8},  // CIRCLED LATIN CAPITAL LETTER S
    {"oscr",             0x02134},  // SCRIPT SMALL O
    {"Oscr",             0x1D4AA},  // MATHEMATICAL SCRIPT CAPITAL O
    {"Oslash",           0x000D8},  // LATIN CAPITAL LETTER O WITH STROKE
    {"oslash",           0x000F8},  // LATIN SMALL LETTER O WITH STROKE
    {"osol",             0x02298},  // CIRCLED DIVISION SLASH
    {"Otilde",           0x000D5},  // LATIN CAPITAL LETTER O WITH TILDE
    {"otilde",           0x000F5},  // LATIN SMALL LETTER O WITH TILDE
    {"otimes",           0x02297},  // CIRCLED TIMES
    {"Otimes",           0x02A37},  // MULTIPLICATION SIGN IN DOUBLE CIRCLE
    {"otimesas",         0x02A36},  // CIRCLED MULTIPLICATION SIGN WITH CIRCUMFLEX ACCENT
    {"Ouml",             0x000D6},  // LATIN CAPITAL LETTER O WITH DIAERESIS
    {"ouml",             0x000F6},  // LATIN SMALL LETTER O WITH DIAERESIS
    {"ovbar",            0x0233D},  // APL FUNCTIONAL SYMBOL CIRCLE STILE
    {"OverBar",          0x0203E},  // OVERLINE
    {"OverBrace",        0x023DE},  // TOP CURLY BRACKET
    {"OverBracket",      0x023B4},  // TOP SQUARE BRACKET
    {"OverParenthesis",  0x023DC},  // TOP PARENTHESIS
];

immutable NameId[] namesP =
[
    {"par",              0x02225},  // PARALLEL TO
    {"para",             0x000B6},  // PILCROW SIGN
    {"parallel",         0x02225},  // PARALLEL TO
    {"parsim",           0x02AF3},  // PARALLEL WITH TILDE OPERATOR
    {"parsl",            0x02AFD},  // DOUBLE SOLIDUS OPERATOR
    {"part",             0x02202},  // PARTIAL DIFFERENTIAL
    {"PartialD",         0x02202},  // PARTIAL DIFFERENTIAL
    {"Pcy",              0x0041F},  // CYRILLIC CAPITAL LETTER PE
    {"pcy",              0x0043F},  // CYRILLIC SMALL LETTER PE
    {"percnt",           0x00025},  // PERCENT SIGN
    {"period",           0x0002E},  // FULL STOP
    {"permil",           0x02030},  // PER MILLE SIGN
    {"perp",             0x022A5},  // UP TACK
    {"pertenk",          0x02031},  // PER TEN THOUSAND SIGN
    {"Pfr",              0x1D513},  // MATHEMATICAL FRAKTUR CAPITAL P
    {"pfr",              0x1D52D},  // MATHEMATICAL FRAKTUR SMALL P
    {"Pgr",              0x003A0},  // GREEK CAPITAL LETTER PI
    {"pgr",              0x003C0},  // GREEK SMALL LETTER PI
    {"PHgr",             0x003A6},  // GREEK CAPITAL LETTER PHI
    {"phgr",             0x003C6},  // GREEK SMALL LETTER PHI
    {"Phi",              0x003A6},  // GREEK CAPITAL LETTER PHI
    {"phi",              0x003C6},  // GREEK SMALL LETTER PHI
    {"phiv",             0x003D5},  // GREEK PHI SYMBOL
    {"phmmat",           0x02133},  // SCRIPT CAPITAL M
    {"phone",            0x0260E},  // BLACK TELEPHONE
    {"Pi",               0x003A0},  // GREEK CAPITAL LETTER PI
    {"pi",               0x003C0},  // GREEK SMALL LETTER PI
    {"pitchfork",        0x022D4},  // PITCHFORK
    {"piv",              0x003D6},  // GREEK PI SYMBOL
    {"planck",           0x0210F},  // PLANCK CONSTANT OVER TWO PI
    {"planckh",          0x0210E},  // PLANCK CONSTANT
    {"plankv",           0x0210F},  // PLANCK CONSTANT OVER TWO PI
    {"plus",             0x0002B},  // PLUS SIGN
    {"plusacir",         0x02A23},  // PLUS SIGN WITH CIRCUMFLEX ACCENT ABOVE
    {"plusb",            0x0229E},  // SQUARED PLUS
    {"pluscir",          0x02A22},  // PLUS SIGN WITH SMALL CIRCLE ABOVE
    {"plusdo",           0x02214},  // DOT PLUS
    {"plusdu",           0x02A25},  // PLUS SIGN WITH DOT BELOW
    {"pluse",            0x02A72},  // PLUS SIGN ABOVE EQUALS SIGN
    {"PlusMinus",        0x000B1},  // PLUS-MINUS SIGN
    {"plusmn",           0x000B1},  // PLUS-MINUS SIGN
    {"plussim",          0x02A26},  // PLUS SIGN WITH TILDE BELOW
    {"plustwo",          0x02A27},  // PLUS SIGN WITH SUBSCRIPT TWO
    {"pm",               0x000B1},  // PLUS-MINUS SIGN
    {"Poincareplane",    0x0210C},  // BLACK-LETTER CAPITAL H
    {"pointint",         0x02A15},  // INTEGRAL AROUND A POINT OPERATOR
    {"Popf",             0x02119},  // DOUBLE-STRUCK CAPITAL P
    {"popf",             0x1D561},  // MATHEMATICAL DOUBLE-STRUCK SMALL P
    {"pound",            0x000A3},  // POUND SIGN
    {"pr",               0x0227A},  // PRECEDES
    {"Pr",               0x02ABB},  // DOUBLE PRECEDES
    {"prap",             0x02AB7},  // PRECEDES ABOVE ALMOST EQUAL TO
    {"prcue",            0x0227C},  // PRECEDES OR EQUAL TO
    {"pre",              0x02AAF},  // PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
    {"prE",              0x02AB3},  // PRECEDES ABOVE EQUALS SIGN
    {"prec",             0x0227A},  // PRECEDES
    {"precapprox",       0x02AB7},  // PRECEDES ABOVE ALMOST EQUAL TO
    {"preccurlyeq",      0x0227C},  // PRECEDES OR EQUAL TO
    {"Precedes",         0x0227A},  // PRECEDES
    {"PrecedesEqual",    0x02AAF},  // PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
    {"PrecedesSlantEqual", 0x0227C},  // PRECEDES OR EQUAL TO
    {"PrecedesTilde",    0x0227E},  // PRECEDES OR EQUIVALENT TO
    {"preceq",           0x02AAF},  // PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
    {"precnapprox",      0x02AB9},  // PRECEDES ABOVE NOT ALMOST EQUAL TO
    {"precneqq",         0x02AB5},  // PRECEDES ABOVE NOT EQUAL TO
    {"precnsim",         0x022E8},  // PRECEDES BUT NOT EQUIVALENT TO
    {"precsim",          0x0227E},  // PRECEDES OR EQUIVALENT TO
    {"prime",            0x02032},  // PRIME
    {"Prime",            0x02033},  // DOUBLE PRIME
    {"primes",           0x02119},  // DOUBLE-STRUCK CAPITAL P
    {"prnap",            0x02AB9},  // PRECEDES ABOVE NOT ALMOST EQUAL TO
    {"prnE",             0x02AB5},  // PRECEDES ABOVE NOT EQUAL TO
    {"prnsim",           0x022E8},  // PRECEDES BUT NOT EQUIVALENT TO
    {"prod",             0x0220F},  // N-ARY PRODUCT
    {"Product",          0x0220F},  // N-ARY PRODUCT
    {"profalar",         0x0232E},  // ALL AROUND-PROFILE
    {"profline",         0x02312},  // ARC
    {"profsurf",         0x02313},  // SEGMENT
    {"prop",             0x0221D},  // PROPORTIONAL TO
    {"Proportion",       0x02237},  // PROPORTION
    {"Proportional",     0x0221D},  // PROPORTIONAL TO
    {"propto",           0x0221D},  // PROPORTIONAL TO
    {"prsim",            0x0227E},  // PRECEDES OR EQUIVALENT TO
    {"prurel",           0x022B0},  // PRECEDES UNDER RELATION
    {"Pscr",             0x1D4AB},  // MATHEMATICAL SCRIPT CAPITAL P
    {"pscr",             0x1D4C5},  // MATHEMATICAL SCRIPT SMALL P
    {"PSgr",             0x003A8},  // GREEK CAPITAL LETTER PSI
    {"psgr",             0x003C8},  // GREEK SMALL LETTER PSI
    {"Psi",              0x003A8},  // GREEK CAPITAL LETTER PSI
    {"psi",              0x003C8},  // GREEK SMALL LETTER PSI
    {"puncsp",           0x02008},  // PUNCTUATION SPACE
];

immutable NameId[] namesQ =
[
    {"Qfr",              0x1D514},  // MATHEMATICAL FRAKTUR CAPITAL Q
    {"qfr",              0x1D52E},  // MATHEMATICAL FRAKTUR SMALL Q
    {"qint",             0x02A0C},  // QUADRUPLE INTEGRAL OPERATOR
    {"Qopf",             0x0211A},  // DOUBLE-STRUCK CAPITAL Q
    {"qopf",             0x1D562},  // MATHEMATICAL DOUBLE-STRUCK SMALL Q
    {"qprime",           0x02057},  // QUADRUPLE PRIME
    {"Qscr",             0x1D4AC},  // MATHEMATICAL SCRIPT CAPITAL Q
    {"qscr",             0x1D4C6},  // MATHEMATICAL SCRIPT SMALL Q
    {"quaternions",      0x0210D},  // DOUBLE-STRUCK CAPITAL H
    {"quatint",          0x02A16},  // QUATERNION INTEGRAL OPERATOR
    {"quest",            0x0003F},  // QUESTION MARK
    {"questeq",          0x0225F},  // QUESTIONED EQUAL TO
    {"quot",             0x00022},  // QUOTATION MARK
    {"QUOT",             0x00022},  // QUOTATION MARK
];

immutable NameId[] namesR =
[
    {"rAarr",            0x021DB},  // RIGHTWARDS TRIPLE ARROW
    {"race",             0x0223D, 0x00331},  // REVERSED TILDE with underline
    {"Racute",           0x00154},  // LATIN CAPITAL LETTER R WITH ACUTE
    {"racute",           0x00155},  // LATIN SMALL LETTER R WITH ACUTE
    {"radic",            0x0221A},  // SQUARE ROOT
    {"raemptyv",         0x029B3},  // EMPTY SET WITH RIGHT ARROW ABOVE
    {"rang",             0x027E9},  // MATHEMATICAL RIGHT ANGLE BRACKET
    {"Rang",             0x027EB},  // MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
    {"rangd",            0x02992},  // RIGHT ANGLE BRACKET WITH DOT
    {"range",            0x029A5},  // REVERSED ANGLE WITH UNDERBAR
    {"rangle",           0x027E9},  // MATHEMATICAL RIGHT ANGLE BRACKET
    {"raquo",            0x000BB},  // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    {"rarr",             0x02192},  // RIGHTWARDS ARROW
    {"Rarr",             0x021A0},  // RIGHTWARDS TWO HEADED ARROW
    {"rArr",             0x021D2},  // RIGHTWARDS DOUBLE ARROW
    {"rarrap",           0x02975},  // RIGHTWARDS ARROW ABOVE ALMOST EQUAL TO
    {"rarrb",            0x021E5},  // RIGHTWARDS ARROW TO BAR
    {"rarrbfs",          0x02920},  // RIGHTWARDS ARROW FROM BAR TO BLACK DIAMOND
    {"rarrc",            0x02933},  // WAVE ARROW POINTING DIRECTLY RIGHT
    {"rarrfs",           0x0291E},  // RIGHTWARDS ARROW TO BLACK DIAMOND
    {"rarrhk",           0x021AA},  // RIGHTWARDS ARROW WITH HOOK
    {"rarrlp",           0x021AC},  // RIGHTWARDS ARROW WITH LOOP
    {"rarrpl",           0x02945},  // RIGHTWARDS ARROW WITH PLUS BELOW
    {"rarrsim",          0x02974},  // RIGHTWARDS ARROW ABOVE TILDE OPERATOR
    {"rarrtl",           0x021A3},  // RIGHTWARDS ARROW WITH TAIL
    {"Rarrtl",           0x02916},  // RIGHTWARDS TWO-HEADED ARROW WITH TAIL
    {"rarrw",            0x0219D},  // RIGHTWARDS WAVE ARROW
    {"ratail",           0x0291A},  // RIGHTWARDS ARROW-TAIL
    {"rAtail",           0x0291C},  // RIGHTWARDS DOUBLE ARROW-TAIL
    {"ratio",            0x02236},  // RATIO
    {"rationals",        0x0211A},  // DOUBLE-STRUCK CAPITAL Q
    {"rbarr",            0x0290D},  // RIGHTWARDS DOUBLE DASH ARROW
    {"rBarr",            0x0290F},  // RIGHTWARDS TRIPLE DASH ARROW
    {"RBarr",            0x02910},  // RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW
    {"rbbrk",            0x02773},  // LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
    {"rbrace",           0x0007D},  // RIGHT CURLY BRACKET
    {"rbrack",           0x0005D},  // RIGHT SQUARE BRACKET
    {"rbrke",            0x0298C},  // RIGHT SQUARE BRACKET WITH UNDERBAR
    {"rbrksld",          0x0298E},  // RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
    {"rbrkslu",          0x02990},  // RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
    {"Rcaron",           0x00158},  // LATIN CAPITAL LETTER R WITH CARON
    {"rcaron",           0x00159},  // LATIN SMALL LETTER R WITH CARON
    {"Rcedil",           0x00156},  // LATIN CAPITAL LETTER R WITH CEDILLA
    {"rcedil",           0x00157},  // LATIN SMALL LETTER R WITH CEDILLA
    {"rceil",            0x02309},  // RIGHT CEILING
    {"rcub",             0x0007D},  // RIGHT CURLY BRACKET
    {"Rcy",              0x00420},  // CYRILLIC CAPITAL LETTER ER
    {"rcy",              0x00440},  // CYRILLIC SMALL LETTER ER
    {"rdca",             0x02937},  // ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS
    {"rdldhar",          0x02969},  // RIGHTWARDS HARPOON WITH BARB DOWN ABOVE LEFTWARDS HARPOON WITH BARB DOWN
    {"rdquo",            0x0201D},  // RIGHT DOUBLE QUOTATION MARK
    {"rdquor",           0x0201D},  // RIGHT DOUBLE QUOTATION MARK
    {"rdsh",             0x021B3},  // DOWNWARDS ARROW WITH TIP RIGHTWARDS
    {"Re",               0x0211C},  // BLACK-LETTER CAPITAL R
    {"real",             0x0211C},  // BLACK-LETTER CAPITAL R
    {"realine",          0x0211B},  // SCRIPT CAPITAL R
    {"realpart",         0x0211C},  // BLACK-LETTER CAPITAL R
    {"reals",            0x0211D},  // DOUBLE-STRUCK CAPITAL R
    {"rect",             0x025AD},  // WHITE RECTANGLE
    {"reg",              0x000AE},  // REGISTERED SIGN
    {"REG",              0x000AE},  // REGISTERED SIGN
    {"ReverseElement",   0x0220B},  // CONTAINS AS MEMBER
    {"ReverseEquilibrium", 0x021CB},  // LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
    {"ReverseUpEquilibrium", 0x0296F},  // DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT
    {"rfisht",           0x0297D},  // RIGHT FISH TAIL
    {"rfloor",           0x0230B},  // RIGHT FLOOR
    {"Rfr",              0x0211C},  // BLACK-LETTER CAPITAL R
    {"rfr",              0x1D52F},  // MATHEMATICAL FRAKTUR SMALL R
    {"Rgr",              0x003A1},  // GREEK CAPITAL LETTER RHO
    {"rgr",              0x003C1},  // GREEK SMALL LETTER RHO
    {"rHar",             0x02964},  // RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN
    {"rhard",            0x021C1},  // RIGHTWARDS HARPOON WITH BARB DOWNWARDS
    {"rharu",            0x021C0},  // RIGHTWARDS HARPOON WITH BARB UPWARDS
    {"rharul",           0x0296C},  // RIGHTWARDS HARPOON WITH BARB UP ABOVE LONG DASH
    {"Rho",              0x003A1},  // GREEK CAPITAL LETTER RHO
    {"rho",              0x003C1},  // GREEK SMALL LETTER RHO
    {"rhov",             0x003F1},  // GREEK RHO SYMBOL
    {"RightAngleBracket", 0x027E9},  // MATHEMATICAL RIGHT ANGLE BRACKET
    {"rightarrow",       0x02192},  // RIGHTWARDS ARROW
    {"RightArrow",       0x02192},  // RIGHTWARDS ARROW
    {"Rightarrow",       0x021D2},  // RIGHTWARDS DOUBLE ARROW
    {"RightArrowBar",    0x021E5},  // RIGHTWARDS ARROW TO BAR
    {"RightArrowLeftArrow", 0x021C4},  // RIGHTWARDS ARROW OVER LEFTWARDS ARROW
    {"rightarrowtail",   0x021A3},  // RIGHTWARDS ARROW WITH TAIL
    {"RightCeiling",     0x02309},  // RIGHT CEILING
    {"RightDoubleBracket", 0x027E7},  // MATHEMATICAL RIGHT WHITE SQUARE BRACKET
    {"RightDownTeeVector", 0x0295D},  // DOWNWARDS HARPOON WITH BARB RIGHT FROM BAR
    {"RightDownVector",  0x021C2},  // DOWNWARDS HARPOON WITH BARB RIGHTWARDS
    {"RightDownVectorBar", 0x02955},  // DOWNWARDS HARPOON WITH BARB RIGHT TO BAR
    {"RightFloor",       0x0230B},  // RIGHT FLOOR
    {"rightharpoondown", 0x021C1},  // RIGHTWARDS HARPOON WITH BARB DOWNWARDS
    {"rightharpoonup",   0x021C0},  // RIGHTWARDS HARPOON WITH BARB UPWARDS
    {"rightleftarrows",  0x021C4},  // RIGHTWARDS ARROW OVER LEFTWARDS ARROW
    {"rightleftharpoons", 0x021CC},  // RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
    {"rightrightarrows", 0x021C9},  // RIGHTWARDS PAIRED ARROWS
    {"rightsquigarrow",  0x0219D},  // RIGHTWARDS WAVE ARROW
    {"RightTee",         0x022A2},  // RIGHT TACK
    {"RightTeeArrow",    0x021A6},  // RIGHTWARDS ARROW FROM BAR
    {"RightTeeVector",   0x0295B},  // RIGHTWARDS HARPOON WITH BARB UP FROM BAR
    {"rightthreetimes",  0x022CC},  // RIGHT SEMIDIRECT PRODUCT
    {"RightTriangle",    0x022B3},  // CONTAINS AS NORMAL SUBGROUP
    {"RightTriangleBar", 0x029D0},  // VERTICAL BAR BESIDE RIGHT TRIANGLE
    {"RightTriangleEqual", 0x022B5},  // CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
    {"RightUpDownVector", 0x0294F},  // UP BARB RIGHT DOWN BARB RIGHT HARPOON
    {"RightUpTeeVector", 0x0295C},  // UPWARDS HARPOON WITH BARB RIGHT FROM BAR
    {"RightUpVector",    0x021BE},  // UPWARDS HARPOON WITH BARB RIGHTWARDS
    {"RightUpVectorBar", 0x02954},  // UPWARDS HARPOON WITH BARB RIGHT TO BAR
    {"RightVector",      0x021C0},  // RIGHTWARDS HARPOON WITH BARB UPWARDS
    {"RightVectorBar",   0x02953},  // RIGHTWARDS HARPOON WITH BARB UP TO BAR
    {"ring",             0x002DA},  // RING ABOVE
    {"risingdotseq",     0x02253},  // IMAGE OF OR APPROXIMATELY EQUAL TO
    {"rlarr",            0x021C4},  // RIGHTWARDS ARROW OVER LEFTWARDS ARROW
    {"rlhar",            0x021CC},  // RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
    {"rlm",              0x0200F},  // RIGHT-TO-LEFT MARK
    {"rmoust",           0x023B1},  // UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION
    {"rmoustache",       0x023B1},  // UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION
    {"rnmid",            0x02AEE},  // DOES NOT DIVIDE WITH REVERSED NEGATION SLASH
    {"roang",            0x027ED},  // MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
    {"roarr",            0x021FE},  // RIGHTWARDS OPEN-HEADED ARROW
    {"robrk",            0x027E7},  // MATHEMATICAL RIGHT WHITE SQUARE BRACKET
    {"ropar",            0x02986},  // RIGHT WHITE PARENTHESIS
    {"Ropf",             0x0211D},  // DOUBLE-STRUCK CAPITAL R
    {"ropf",             0x1D563},  // MATHEMATICAL DOUBLE-STRUCK SMALL R
    {"roplus",           0x02A2E},  // PLUS SIGN IN RIGHT HALF CIRCLE
    {"rotimes",          0x02A35},  // MULTIPLICATION SIGN IN RIGHT HALF CIRCLE
    {"RoundImplies",     0x02970},  // RIGHT DOUBLE ARROW WITH ROUNDED HEAD
    {"rpar",             0x00029},  // RIGHT PARENTHESIS
    {"rpargt",           0x02994},  // RIGHT ARC GREATER-THAN BRACKET
    {"rppolint",         0x02A12},  // LINE INTEGRATION WITH RECTANGULAR PATH AROUND POLE
    {"rrarr",            0x021C9},  // RIGHTWARDS PAIRED ARROWS
    {"Rrightarrow",      0x021DB},  // RIGHTWARDS TRIPLE ARROW
    {"rsaquo",           0x0203A},  // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    {"Rscr",             0x0211B},  // SCRIPT CAPITAL R
    {"rscr",             0x1D4C7},  // MATHEMATICAL SCRIPT SMALL R
    {"rsh",              0x021B1},  // UPWARDS ARROW WITH TIP RIGHTWARDS
    {"Rsh",              0x021B1},  // UPWARDS ARROW WITH TIP RIGHTWARDS
    {"rsqb",             0x0005D},  // RIGHT SQUARE BRACKET
    {"rsquo",            0x02019},  // RIGHT SINGLE QUOTATION MARK
    {"rsquor",           0x02019},  // RIGHT SINGLE QUOTATION MARK
    {"rthree",           0x022CC},  // RIGHT SEMIDIRECT PRODUCT
    {"rtimes",           0x022CA},  // RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
    {"rtri",             0x025B9},  // WHITE RIGHT-POINTING SMALL TRIANGLE
    {"rtrie",            0x022B5},  // CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
    {"rtrif",            0x025B8},  // BLACK RIGHT-POINTING SMALL TRIANGLE
    {"rtriltri",         0x029CE},  // RIGHT TRIANGLE ABOVE LEFT TRIANGLE
    {"RuleDelayed",      0x029F4},  // RULE-DELAYED
    {"ruluhar",          0x02968},  // RIGHTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB UP
    {"rx",               0x0211E},  // PRESCRIPTION TAKE
];

immutable NameId[] namesS =
[
    {"Sacute",           0x0015A},  // LATIN CAPITAL LETTER S WITH ACUTE
    {"sacute",           0x0015B},  // LATIN SMALL LETTER S WITH ACUTE
    {"sbquo",            0x0201A},  // SINGLE LOW-9 QUOTATION MARK
    {"sc",               0x0227B},  // SUCCEEDS
    {"Sc",               0x02ABC},  // DOUBLE SUCCEEDS
    {"scap",             0x02AB8},  // SUCCEEDS ABOVE ALMOST EQUAL TO
    {"Scaron",           0x00160},  // LATIN CAPITAL LETTER S WITH CARON
    {"scaron",           0x00161},  // LATIN SMALL LETTER S WITH CARON
    {"sccue",            0x0227D},  // SUCCEEDS OR EQUAL TO
    {"sce",              0x02AB0},  // SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
    {"scE",              0x02AB4},  // SUCCEEDS ABOVE EQUALS SIGN
    {"Scedil",           0x0015E},  // LATIN CAPITAL LETTER S WITH CEDILLA
    {"scedil",           0x0015F},  // LATIN SMALL LETTER S WITH CEDILLA
    {"Scirc",            0x0015C},  // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    {"scirc",            0x0015D},  // LATIN SMALL LETTER S WITH CIRCUMFLEX
    {"scnap",            0x02ABA},  // SUCCEEDS ABOVE NOT ALMOST EQUAL TO
    {"scnE",             0x02AB6},  // SUCCEEDS ABOVE NOT EQUAL TO
    {"scnsim",           0x022E9},  // SUCCEEDS BUT NOT EQUIVALENT TO
    {"scpolint",         0x02A13},  // LINE INTEGRATION WITH SEMICIRCULAR PATH AROUND POLE
    {"scsim",            0x0227F},  // SUCCEEDS OR EQUIVALENT TO
    {"Scy",              0x00421},  // CYRILLIC CAPITAL LETTER ES
    {"scy",              0x00441},  // CYRILLIC SMALL LETTER ES
    {"sdot",             0x022C5},  // DOT OPERATOR
    {"sdotb",            0x022A1},  // SQUARED DOT OPERATOR
    {"sdote",            0x02A66},  // EQUALS SIGN WITH DOT BELOW
    {"searhk",           0x02925},  // SOUTH EAST ARROW WITH HOOK
    {"searr",            0x02198},  // SOUTH EAST ARROW
    {"seArr",            0x021D8},  // SOUTH EAST DOUBLE ARROW
    {"searrow",          0x02198},  // SOUTH EAST ARROW
    {"sect",             0x000A7},  // SECTION SIGN
    {"semi",             0x0003B},  // SEMICOLON
    {"seswar",           0x02929},  // SOUTH EAST ARROW AND SOUTH WEST ARROW
    {"setminus",         0x02216},  // SET MINUS
    {"setmn",            0x02216},  // SET MINUS
    {"sext",             0x02736},  // SIX POINTED BLACK STAR
    {"sfgr",             0x003C2},  // GREEK SMALL LETTER FINAL SIGMA
    {"Sfr",              0x1D516},  // MATHEMATICAL FRAKTUR CAPITAL S
    {"sfr",              0x1D530},  // MATHEMATICAL FRAKTUR SMALL S
    {"sfrown",           0x02322},  // FROWN
    {"Sgr",              0x003A3},  // GREEK CAPITAL LETTER SIGMA
    {"sgr",              0x003C3},  // GREEK SMALL LETTER SIGMA
    {"sharp",            0x0266F},  // MUSIC SHARP SIGN
    {"SHCHcy",           0x00429},  // CYRILLIC CAPITAL LETTER SHCHA
    {"shchcy",           0x00449},  // CYRILLIC SMALL LETTER SHCHA
    {"SHcy",             0x00428},  // CYRILLIC CAPITAL LETTER SHA
    {"shcy",             0x00448},  // CYRILLIC SMALL LETTER SHA
    {"ShortDownArrow",   0x02193},  // DOWNWARDS ARROW
    {"ShortLeftArrow",   0x02190},  // LEFTWARDS ARROW
    {"shortmid",         0x02223},  // DIVIDES
    {"shortparallel",    0x02225},  // PARALLEL TO
    {"ShortRightArrow",  0x02192},  // RIGHTWARDS ARROW
    {"ShortUpArrow",     0x02191},  // UPWARDS ARROW
    {"shy",              0x000AD},  // SOFT HYPHEN
    {"Sigma",            0x003A3},  // GREEK CAPITAL LETTER SIGMA
    {"sigma",            0x003C3},  // GREEK SMALL LETTER SIGMA
    {"sigmaf",           0x003C2},  // GREEK SMALL LETTER FINAL SIGMA
    {"sigmav",           0x003C2},  // GREEK SMALL LETTER FINAL SIGMA
    {"sim",              0x0223C},  // TILDE OPERATOR
    {"simdot",           0x02A6A},  // TILDE OPERATOR WITH DOT ABOVE
    {"sime",             0x02243},  // ASYMPTOTICALLY EQUAL TO
    {"simeq",            0x02243},  // ASYMPTOTICALLY EQUAL TO
    {"simg",             0x02A9E},  // SIMILAR OR GREATER-THAN
    {"simgE",            0x02AA0},  // SIMILAR ABOVE GREATER-THAN ABOVE EQUALS SIGN
    {"siml",             0x02A9D},  // SIMILAR OR LESS-THAN
    {"simlE",            0x02A9F},  // SIMILAR ABOVE LESS-THAN ABOVE EQUALS SIGN
    {"simne",            0x02246},  // APPROXIMATELY BUT NOT ACTUALLY EQUAL TO
    {"simplus",          0x02A24},  // PLUS SIGN WITH TILDE ABOVE
    {"simrarr",          0x02972},  // TILDE OPERATOR ABOVE RIGHTWARDS ARROW
    {"slarr",            0x02190},  // LEFTWARDS ARROW
    {"SmallCircle",      0x02218},  // RING OPERATOR
    {"smallsetminus",    0x02216},  // SET MINUS
    {"smashp",           0x02A33},  // SMASH PRODUCT
    {"smeparsl",         0x029E4},  // EQUALS SIGN AND SLANTED PARALLEL WITH TILDE ABOVE
    {"smid",             0x02223},  // DIVIDES
    {"smile",            0x02323},  // SMILE
    {"smt",              0x02AAA},  // SMALLER THAN
    {"smte",             0x02AAC},  // SMALLER THAN OR EQUAL TO
    {"smtes",            0x02AAC, 0x0FE00},  // SMALLER THAN OR slanted EQUAL
    {"SOFTcy",           0x0042C},  // CYRILLIC CAPITAL LETTER SOFT SIGN
    {"softcy",           0x0044C},  // CYRILLIC SMALL LETTER SOFT SIGN
    {"sol",              0x0002F},  // SOLIDUS
    {"solb",             0x029C4},  // SQUARED RISING DIAGONAL SLASH
    {"solbar",           0x0233F},  // APL FUNCTIONAL SYMBOL SLASH BAR
    {"Sopf",             0x1D54A},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL S
    {"sopf",             0x1D564},  // MATHEMATICAL DOUBLE-STRUCK SMALL S
    {"spades",           0x02660},  // BLACK SPADE SUIT
    {"spadesuit",        0x02660},  // BLACK SPADE SUIT
    {"spar",             0x02225},  // PARALLEL TO
    {"sqcap",            0x02293},  // SQUARE CAP
    {"sqcaps",           0x02293, 0x0FE00},  // SQUARE CAP with serifs
    {"sqcup",            0x02294},  // SQUARE CUP
    {"sqcups",           0x02294, 0x0FE00},  // SQUARE CUP with serifs
    {"Sqrt",             0x0221A},  // SQUARE ROOT
    {"sqsub",            0x0228F},  // SQUARE IMAGE OF
    {"sqsube",           0x02291},  // SQUARE IMAGE OF OR EQUAL TO
    {"sqsubset",         0x0228F},  // SQUARE IMAGE OF
    {"sqsubseteq",       0x02291},  // SQUARE IMAGE OF OR EQUAL TO
    {"sqsup",            0x02290},  // SQUARE ORIGINAL OF
    {"sqsupe",           0x02292},  // SQUARE ORIGINAL OF OR EQUAL TO
    {"sqsupset",         0x02290},  // SQUARE ORIGINAL OF
    {"sqsupseteq",       0x02292},  // SQUARE ORIGINAL OF OR EQUAL TO
    {"squ",              0x025A1},  // WHITE SQUARE
    {"square",           0x025A1},  // WHITE SQUARE
    {"Square",           0x025A1},  // WHITE SQUARE
    {"SquareIntersection", 0x02293},  // SQUARE CAP
    {"SquareSubset",     0x0228F},  // SQUARE IMAGE OF
    {"SquareSubsetEqual", 0x02291},  // SQUARE IMAGE OF OR EQUAL TO
    {"SquareSuperset",   0x02290},  // SQUARE ORIGINAL OF
    {"SquareSupersetEqual", 0x02292},  // SQUARE ORIGINAL OF OR EQUAL TO
    {"SquareUnion",      0x02294},  // SQUARE CUP
    {"squarf",           0x025AA},  // BLACK SMALL SQUARE
    {"squf",             0x025AA},  // BLACK SMALL SQUARE
    {"srarr",            0x02192},  // RIGHTWARDS ARROW
    {"Sscr",             0x1D4AE},  // MATHEMATICAL SCRIPT CAPITAL S
    {"sscr",             0x1D4C8},  // MATHEMATICAL SCRIPT SMALL S
    {"ssetmn",           0x02216},  // SET MINUS
    {"ssmile",           0x02323},  // SMILE
    {"sstarf",           0x022C6},  // STAR OPERATOR
    {"Star",             0x022C6},  // STAR OPERATOR
    {"star",             0x02606},  // WHITE STAR
    {"starf",            0x02605},  // BLACK STAR
    {"straightepsilon",  0x003F5},  // GREEK LUNATE EPSILON SYMBOL
    {"straightphi",      0x003D5},  // GREEK PHI SYMBOL
    {"strns",            0x000AF},  // MACRON
    {"sub",              0x02282},  // SUBSET OF
    {"Sub",              0x022D0},  // DOUBLE SUBSET
    {"subdot",           0x02ABD},  // SUBSET WITH DOT
    {"sube",             0x02286},  // SUBSET OF OR EQUAL TO
    {"subE",             0x02AC5},  // SUBSET OF ABOVE EQUALS SIGN
    {"subedot",          0x02AC3},  // SUBSET OF OR EQUAL TO WITH DOT ABOVE
    {"submult",          0x02AC1},  // SUBSET WITH MULTIPLICATION SIGN BELOW
    {"subne",            0x0228A},  // SUBSET OF WITH NOT EQUAL TO
    {"subnE",            0x02ACB},  // SUBSET OF ABOVE NOT EQUAL TO
    {"subplus",          0x02ABF},  // SUBSET WITH PLUS SIGN BELOW
    {"subrarr",          0x02979},  // SUBSET ABOVE RIGHTWARDS ARROW
    {"subset",           0x02282},  // SUBSET OF
    {"Subset",           0x022D0},  // DOUBLE SUBSET
    {"subseteq",         0x02286},  // SUBSET OF OR EQUAL TO
    {"subseteqq",        0x02AC5},  // SUBSET OF ABOVE EQUALS SIGN
    {"SubsetEqual",      0x02286},  // SUBSET OF OR EQUAL TO
    {"subsetneq",        0x0228A},  // SUBSET OF WITH NOT EQUAL TO
    {"subsetneqq",       0x02ACB},  // SUBSET OF ABOVE NOT EQUAL TO
    {"subsim",           0x02AC7},  // SUBSET OF ABOVE TILDE OPERATOR
    {"subsub",           0x02AD5},  // SUBSET ABOVE SUBSET
    {"subsup",           0x02AD3},  // SUBSET ABOVE SUPERSET
    {"succ",             0x0227B},  // SUCCEEDS
    {"succapprox",       0x02AB8},  // SUCCEEDS ABOVE ALMOST EQUAL TO
    {"succcurlyeq",      0x0227D},  // SUCCEEDS OR EQUAL TO
    {"Succeeds",         0x0227B},  // SUCCEEDS
    {"SucceedsEqual",    0x02AB0},  // SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
    {"SucceedsSlantEqual", 0x0227D},  // SUCCEEDS OR EQUAL TO
    {"SucceedsTilde",    0x0227F},  // SUCCEEDS OR EQUIVALENT TO
    {"succeq",           0x02AB0},  // SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
    {"succnapprox",      0x02ABA},  // SUCCEEDS ABOVE NOT ALMOST EQUAL TO
    {"succneqq",         0x02AB6},  // SUCCEEDS ABOVE NOT EQUAL TO
    {"succnsim",         0x022E9},  // SUCCEEDS BUT NOT EQUIVALENT TO
    {"succsim",          0x0227F},  // SUCCEEDS OR EQUIVALENT TO
    {"SuchThat",         0x0220B},  // CONTAINS AS MEMBER
    {"sum",              0x02211},  // N-ARY SUMMATION
    {"Sum",              0x02211},  // N-ARY SUMMATION
    {"sung",             0x0266A},  // EIGHTH NOTE
    {"sup",              0x02283},  // SUPERSET OF
    {"Sup",              0x022D1},  // DOUBLE SUPERSET
    {"sup1",             0x000B9},  // SUPERSCRIPT ONE
    {"sup2",             0x000B2},  // SUPERSCRIPT TWO
    {"sup3",             0x000B3},  // SUPERSCRIPT THREE
    {"supdot",           0x02ABE},  // SUPERSET WITH DOT
    {"supdsub",          0x02AD8},  // SUPERSET BESIDE AND JOINED BY DASH WITH SUBSET
    {"supe",             0x02287},  // SUPERSET OF OR EQUAL TO
    {"supE",             0x02AC6},  // SUPERSET OF ABOVE EQUALS SIGN
    {"supedot",          0x02AC4},  // SUPERSET OF OR EQUAL TO WITH DOT ABOVE
    {"Superset",         0x02283},  // SUPERSET OF
    {"SupersetEqual",    0x02287},  // SUPERSET OF OR EQUAL TO
    {"suphsol",          0x027C9},  // SUPERSET PRECEDING SOLIDUS
    {"suphsub",          0x02AD7},  // SUPERSET BESIDE SUBSET
    {"suplarr",          0x0297B},  // SUPERSET ABOVE LEFTWARDS ARROW
    {"supmult",          0x02AC2},  // SUPERSET WITH MULTIPLICATION SIGN BELOW
    {"supne",            0x0228B},  // SUPERSET OF WITH NOT EQUAL TO
    {"supnE",            0x02ACC},  // SUPERSET OF ABOVE NOT EQUAL TO
    {"supplus",          0x02AC0},  // SUPERSET WITH PLUS SIGN BELOW
    {"supset",           0x02283},  // SUPERSET OF
    {"Supset",           0x022D1},  // DOUBLE SUPERSET
    {"supseteq",         0x02287},  // SUPERSET OF OR EQUAL TO
    {"supseteqq",        0x02AC6},  // SUPERSET OF ABOVE EQUALS SIGN
    {"supsetneq",        0x0228B},  // SUPERSET OF WITH NOT EQUAL TO
    {"supsetneqq",       0x02ACC},  // SUPERSET OF ABOVE NOT EQUAL TO
    {"supsim",           0x02AC8},  // SUPERSET OF ABOVE TILDE OPERATOR
    {"supsub",           0x02AD4},  // SUPERSET ABOVE SUBSET
    {"supsup",           0x02AD6},  // SUPERSET ABOVE SUPERSET
    {"swarhk",           0x02926},  // SOUTH WEST ARROW WITH HOOK
    {"swarr",            0x02199},  // SOUTH WEST ARROW
    {"swArr",            0x021D9},  // SOUTH WEST DOUBLE ARROW
    {"swarrow",          0x02199},  // SOUTH WEST ARROW
    {"swnwar",           0x0292A},  // SOUTH WEST ARROW AND NORTH WEST ARROW
    {"szlig",            0x000DF},  // LATIN SMALL LETTER SHARP S
];

immutable NameId[] namesT =
[
    {"Tab",              0x00009},  // CHARACTER TABULATION
    {"target",           0x02316},  // POSITION INDICATOR
    {"Tau",              0x003A4},  // GREEK CAPITAL LETTER TAU
    {"tau",              0x003C4},  // GREEK SMALL LETTER TAU
    {"tbrk",             0x023B4},  // TOP SQUARE BRACKET
    {"Tcaron",           0x00164},  // LATIN CAPITAL LETTER T WITH CARON
    {"tcaron",           0x00165},  // LATIN SMALL LETTER T WITH CARON
    {"Tcedil",           0x00162},  // LATIN CAPITAL LETTER T WITH CEDILLA
    {"tcedil",           0x00163},  // LATIN SMALL LETTER T WITH CEDILLA
    {"Tcy",              0x00422},  // CYRILLIC CAPITAL LETTER TE
    {"tcy",              0x00442},  // CYRILLIC SMALL LETTER TE
    {"tdot",             0x020DB},  // COMBINING THREE DOTS ABOVE
    {"telrec",           0x02315},  // TELEPHONE RECORDER
    {"Tfr",              0x1D517},  // MATHEMATICAL FRAKTUR CAPITAL T
    {"tfr",              0x1D531},  // MATHEMATICAL FRAKTUR SMALL T
    {"Tgr",              0x003A4},  // GREEK CAPITAL LETTER TAU
    {"tgr",              0x003C4},  // GREEK SMALL LETTER TAU
    {"there4",           0x02234},  // THEREFORE
    {"therefore",        0x02234},  // THEREFORE
    {"Therefore",        0x02234},  // THEREFORE
    {"Theta",            0x00398},  // GREEK CAPITAL LETTER THETA
    {"theta",            0x003B8},  // GREEK SMALL LETTER THETA
    {"thetasym",         0x003D1},  // GREEK THETA SYMBOL
    {"thetav",           0x003D1},  // GREEK THETA SYMBOL
    {"THgr",             0x00398},  // GREEK CAPITAL LETTER THETA
    {"thgr",             0x003B8},  // GREEK SMALL LETTER THETA
    {"thickapprox",      0x02248},  // ALMOST EQUAL TO
    {"thicksim",         0x0223C},  // TILDE OPERATOR
    {"ThickSpace",       0x0205F, 0x0200A},  // space of width 5/18 em
    {"thinsp",           0x02009},  // THIN SPACE
    {"ThinSpace",        0x02009},  // THIN SPACE
    {"thkap",            0x02248},  // ALMOST EQUAL TO
    {"thksim",           0x0223C},  // TILDE OPERATOR
    {"THORN",            0x000DE},  // LATIN CAPITAL LETTER THORN
    {"thorn",            0x000FE},  // LATIN SMALL LETTER THORN
    {"tilde",            0x002DC},  // SMALL TILDE
    {"Tilde",            0x0223C},  // TILDE OPERATOR
    {"TildeEqual",       0x02243},  // ASYMPTOTICALLY EQUAL TO
    {"TildeFullEqual",   0x02245},  // APPROXIMATELY EQUAL TO
    {"TildeTilde",       0x02248},  // ALMOST EQUAL TO
    {"times",            0x000D7},  // MULTIPLICATION SIGN
    {"timesb",           0x022A0},  // SQUARED TIMES
    {"timesbar",         0x02A31},  // MULTIPLICATION SIGN WITH UNDERBAR
    {"timesd",           0x02A30},  // MULTIPLICATION SIGN WITH DOT ABOVE
    {"tint",             0x0222D},  // TRIPLE INTEGRAL
    {"toea",             0x02928},  // NORTH EAST ARROW AND SOUTH EAST ARROW
    {"top",              0x022A4},  // DOWN TACK
    {"topbot",           0x02336},  // APL FUNCTIONAL SYMBOL I-BEAM
    {"topcir",           0x02AF1},  // DOWN TACK WITH CIRCLE BELOW
    {"Topf",             0x1D54B},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL T
    {"topf",             0x1D565},  // MATHEMATICAL DOUBLE-STRUCK SMALL T
    {"topfork",          0x02ADA},  // PITCHFORK WITH TEE TOP
    {"tosa",             0x02929},  // SOUTH EAST ARROW AND SOUTH WEST ARROW
    {"tprime",           0x02034},  // TRIPLE PRIME
    {"trade",            0x02122},  // TRADE MARK SIGN
    {"TRADE",            0x02122},  // TRADE MARK SIGN
    {"triangle",         0x025B5},  // WHITE UP-POINTING SMALL TRIANGLE
    {"triangledown",     0x025BF},  // WHITE DOWN-POINTING SMALL TRIANGLE
    {"triangleleft",     0x025C3},  // WHITE LEFT-POINTING SMALL TRIANGLE
    {"trianglelefteq",   0x022B4},  // NORMAL SUBGROUP OF OR EQUAL TO
    {"triangleq",        0x0225C},  // DELTA EQUAL TO
    {"triangleright",    0x025B9},  // WHITE RIGHT-POINTING SMALL TRIANGLE
    {"trianglerighteq",  0x022B5},  // CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
    {"tridot",           0x025EC},  // WHITE UP-POINTING TRIANGLE WITH DOT
    {"trie",             0x0225C},  // DELTA EQUAL TO
    {"triminus",         0x02A3A},  // MINUS SIGN IN TRIANGLE
    {"TripleDot",        0x020DB},  // COMBINING THREE DOTS ABOVE
    {"triplus",          0x02A39},  // PLUS SIGN IN TRIANGLE
    {"trisb",            0x029CD},  // TRIANGLE WITH SERIFS AT BOTTOM
    {"tritime",          0x02A3B},  // MULTIPLICATION SIGN IN TRIANGLE
    {"trpezium",         0x023E2},  // WHITE TRAPEZIUM
    {"Tscr",             0x1D4AF},  // MATHEMATICAL SCRIPT CAPITAL T
    {"tscr",             0x1D4C9},  // MATHEMATICAL SCRIPT SMALL T
    {"TScy",             0x00426},  // CYRILLIC CAPITAL LETTER TSE
    {"tscy",             0x00446},  // CYRILLIC SMALL LETTER TSE
    {"TSHcy",            0x0040B},  // CYRILLIC CAPITAL LETTER TSHE
    {"tshcy",            0x0045B},  // CYRILLIC SMALL LETTER TSHE
    {"Tstrok",           0x00166},  // LATIN CAPITAL LETTER T WITH STROKE
    {"tstrok",           0x00167},  // LATIN SMALL LETTER T WITH STROKE
    {"twixt",            0x0226C},  // BETWEEN
    {"twoheadleftarrow", 0x0219E},  // LEFTWARDS TWO HEADED ARROW
    {"twoheadrightarrow", 0x021A0},  // RIGHTWARDS TWO HEADED ARROW
];

immutable NameId[] namesU =
[
    {"Uacgr",            0x0038E},  // GREEK CAPITAL LETTER UPSILON WITH TONOS
    {"uacgr",            0x003CD},  // GREEK SMALL LETTER UPSILON WITH TONOS
    {"Uacute",           0x000DA},  // LATIN CAPITAL LETTER U WITH ACUTE
    {"uacute",           0x000FA},  // LATIN SMALL LETTER U WITH ACUTE
    {"uarr",             0x02191},  // UPWARDS ARROW
    {"Uarr",             0x0219F},  // UPWARDS TWO HEADED ARROW
    {"uArr",             0x021D1},  // UPWARDS DOUBLE ARROW
    {"Uarrocir",         0x02949},  // UPWARDS TWO-HEADED ARROW FROM SMALL CIRCLE
    {"Ubrcy",            0x0040E},  // CYRILLIC CAPITAL LETTER SHORT U
    {"ubrcy",            0x0045E},  // CYRILLIC SMALL LETTER SHORT U
    {"Ubreve",           0x0016C},  // LATIN CAPITAL LETTER U WITH BREVE
    {"ubreve",           0x0016D},  // LATIN SMALL LETTER U WITH BREVE
    {"Ucirc",            0x000DB},  // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    {"ucirc",            0x000FB},  // LATIN SMALL LETTER U WITH CIRCUMFLEX
    {"Ucy",              0x00423},  // CYRILLIC CAPITAL LETTER U
    {"ucy",              0x00443},  // CYRILLIC SMALL LETTER U
    {"udarr",            0x021C5},  // UPWARDS ARROW LEFTWARDS OF DOWNWARDS ARROW
    {"Udblac",           0x00170},  // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    {"udblac",           0x00171},  // LATIN SMALL LETTER U WITH DOUBLE ACUTE
    {"udhar",            0x0296E},  // UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT
    {"udiagr",           0x003B0},  // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    {"Udigr",            0x003AB},  // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    {"udigr",            0x003CB},  // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    {"ufisht",           0x0297E},  // UP FISH TAIL
    {"Ufr",              0x1D518},  // MATHEMATICAL FRAKTUR CAPITAL U
    {"ufr",              0x1D532},  // MATHEMATICAL FRAKTUR SMALL U
    {"Ugr",              0x003A5},  // GREEK CAPITAL LETTER UPSILON
    {"ugr",              0x003C5},  // GREEK SMALL LETTER UPSILON
    {"Ugrave",           0x000D9},  // LATIN CAPITAL LETTER U WITH GRAVE
    {"ugrave",           0x000F9},  // LATIN SMALL LETTER U WITH GRAVE
    {"uHar",             0x02963},  // UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT
    {"uharl",            0x021BF},  // UPWARDS HARPOON WITH BARB LEFTWARDS
    {"uharr",            0x021BE},  // UPWARDS HARPOON WITH BARB RIGHTWARDS
    {"uhblk",            0x02580},  // UPPER HALF BLOCK
    {"ulcorn",           0x0231C},  // TOP LEFT CORNER
    {"ulcorner",         0x0231C},  // TOP LEFT CORNER
    {"ulcrop",           0x0230F},  // TOP LEFT CROP
    {"ultri",            0x025F8},  // UPPER LEFT TRIANGLE
    {"Umacr",            0x0016A},  // LATIN CAPITAL LETTER U WITH MACRON
    {"umacr",            0x0016B},  // LATIN SMALL LETTER U WITH MACRON
    {"uml",              0x000A8},  // DIAERESIS
    {"UnderBar",         0x0005F},  // LOW LINE
    {"UnderBrace",       0x023DF},  // BOTTOM CURLY BRACKET
    {"UnderBracket",     0x023B5},  // BOTTOM SQUARE BRACKET
    {"UnderParenthesis", 0x023DD},  // BOTTOM PARENTHESIS
    {"Union",            0x022C3},  // N-ARY UNION
    {"UnionPlus",        0x0228E},  // MULTISET UNION
    {"Uogon",            0x00172},  // LATIN CAPITAL LETTER U WITH OGONEK
    {"uogon",            0x00173},  // LATIN SMALL LETTER U WITH OGONEK
    {"Uopf",             0x1D54C},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL U
    {"uopf",             0x1D566},  // MATHEMATICAL DOUBLE-STRUCK SMALL U
    {"uparrow",          0x02191},  // UPWARDS ARROW
    {"UpArrow",          0x02191},  // UPWARDS ARROW
    {"Uparrow",          0x021D1},  // UPWARDS DOUBLE ARROW
    {"UpArrowBar",       0x02912},  // UPWARDS ARROW TO BAR
    {"UpArrowDownArrow", 0x021C5},  // UPWARDS ARROW LEFTWARDS OF DOWNWARDS ARROW
    {"updownarrow",      0x02195},  // UP DOWN ARROW
    {"UpDownArrow",      0x02195},  // UP DOWN ARROW
    {"Updownarrow",      0x021D5},  // UP DOWN DOUBLE ARROW
    {"UpEquilibrium",    0x0296E},  // UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT
    {"upharpoonleft",    0x021BF},  // UPWARDS HARPOON WITH BARB LEFTWARDS
    {"upharpoonright",   0x021BE},  // UPWARDS HARPOON WITH BARB RIGHTWARDS
    {"uplus",            0x0228E},  // MULTISET UNION
    {"UpperLeftArrow",   0x02196},  // NORTH WEST ARROW
    {"UpperRightArrow",  0x02197},  // NORTH EAST ARROW
    {"upsi",             0x003C5},  // GREEK SMALL LETTER UPSILON
    {"Upsi",             0x003D2},  // GREEK UPSILON WITH HOOK SYMBOL
    {"upsih",            0x003D2},  // GREEK UPSILON WITH HOOK SYMBOL
    {"Upsilon",          0x003A5},  // GREEK CAPITAL LETTER UPSILON
    {"upsilon",          0x003C5},  // GREEK SMALL LETTER UPSILON
    {"UpTee",            0x022A5},  // UP TACK
    {"UpTeeArrow",       0x021A5},  // UPWARDS ARROW FROM BAR
    {"upuparrows",       0x021C8},  // UPWARDS PAIRED ARROWS
    {"urcorn",           0x0231D},  // TOP RIGHT CORNER
    {"urcorner",         0x0231D},  // TOP RIGHT CORNER
    {"urcrop",           0x0230E},  // TOP RIGHT CROP
    {"Uring",            0x0016E},  // LATIN CAPITAL LETTER U WITH RING ABOVE
    {"uring",            0x0016F},  // LATIN SMALL LETTER U WITH RING ABOVE
    {"urtri",            0x025F9},  // UPPER RIGHT TRIANGLE
    {"Uscr",             0x1D4B0},  // MATHEMATICAL SCRIPT CAPITAL U
    {"uscr",             0x1D4CA},  // MATHEMATICAL SCRIPT SMALL U
    {"utdot",            0x022F0},  // UP RIGHT DIAGONAL ELLIPSIS
    {"Utilde",           0x00168},  // LATIN CAPITAL LETTER U WITH TILDE
    {"utilde",           0x00169},  // LATIN SMALL LETTER U WITH TILDE
    {"utri",             0x025B5},  // WHITE UP-POINTING SMALL TRIANGLE
    {"utrif",            0x025B4},  // BLACK UP-POINTING SMALL TRIANGLE
    {"uuarr",            0x021C8},  // UPWARDS PAIRED ARROWS
    {"Uuml",             0x000DC},  // LATIN CAPITAL LETTER U WITH DIAERESIS
    {"uuml",             0x000FC},  // LATIN SMALL LETTER U WITH DIAERESIS
    {"uwangle",          0x029A7},  // OBLIQUE ANGLE OPENING DOWN
];

immutable NameId[] namesV =
[
    {"vangrt",           0x0299C},  // RIGHT ANGLE VARIANT WITH SQUARE
    {"varepsilon",       0x003F5},  // GREEK LUNATE EPSILON SYMBOL
    {"varkappa",         0x003F0},  // GREEK KAPPA SYMBOL
    {"varnothing",       0x02205},  // EMPTY SET
    {"varphi",           0x003D5},  // GREEK PHI SYMBOL
    {"varpi",            0x003D6},  // GREEK PI SYMBOL
    {"varpropto",        0x0221D},  // PROPORTIONAL TO
    {"varr",             0x02195},  // UP DOWN ARROW
    {"vArr",             0x021D5},  // UP DOWN DOUBLE ARROW
    {"varrho",           0x003F1},  // GREEK RHO SYMBOL
    {"varsigma",         0x003C2},  // GREEK SMALL LETTER FINAL SIGMA
    {"varsubsetneq",     0x0228A, 0x0FE00},  // SUBSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
    {"varsubsetneqq",    0x02ACB, 0x0FE00},  // SUBSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
    {"varsupsetneq",     0x0228B, 0x0FE00},  // SUPERSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
    {"varsupsetneqq",    0x02ACC, 0x0FE00},  // SUPERSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
    {"vartheta",         0x003D1},  // GREEK THETA SYMBOL
    {"vartriangleleft",  0x022B2},  // NORMAL SUBGROUP OF
    {"vartriangleright", 0x022B3},  // CONTAINS AS NORMAL SUBGROUP
    {"vBar",             0x02AE8},  // SHORT UP TACK WITH UNDERBAR
    {"Vbar",             0x02AEB},  // DOUBLE UP TACK
    {"vBarv",            0x02AE9},  // SHORT UP TACK ABOVE SHORT DOWN TACK
    {"Vcy",              0x00412},  // CYRILLIC CAPITAL LETTER VE
    {"vcy",              0x00432},  // CYRILLIC SMALL LETTER VE
    {"vdash",            0x022A2},  // RIGHT TACK
    {"vDash",            0x022A8},  // TRUE
    {"Vdash",            0x022A9},  // FORCES
    {"VDash",            0x022AB},  // DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
    {"Vdashl",           0x02AE6},  // LONG DASH FROM LEFT MEMBER OF DOUBLE VERTICAL
    {"vee",              0x02228},  // LOGICAL OR
    {"Vee",              0x022C1},  // N-ARY LOGICAL OR
    {"veebar",           0x022BB},  // XOR
    {"veeeq",            0x0225A},  // EQUIANGULAR TO
    {"vellip",           0x022EE},  // VERTICAL ELLIPSIS
    {"verbar",           0x0007C},  // VERTICAL LINE
    {"Verbar",           0x02017},  // DOUBLE VERTICAL LINE
    {"vert",             0x0007C},  // VERTICAL LINE
    {"Vert",             0x02017},  // DOUBLE VERTICAL LINE
    {"VerticalBar",      0x02223},  // DIVIDES
    {"VerticalLine",     0x0007C},  // VERTICAL LINE
    {"VerticalSeparator", 0x02758},  // LIGHT VERTICAL BAR
    {"VerticalTilde",    0x02240},  // WREATH PRODUCT
    {"VeryThinSpace",    0x0200A},  // HAIR SPACE
    {"Vfr",              0x1D519},  // MATHEMATICAL FRAKTUR CAPITAL V
    {"vfr",              0x1D533},  // MATHEMATICAL FRAKTUR SMALL V
    {"vltri",            0x022B2},  // NORMAL SUBGROUP OF
    {"vnsub",            0x02282, 0x020D2},  // SUBSET OF with vertical line
    {"vnsup",            0x02283, 0x020D2},  // SUPERSET OF with vertical line
    {"Vopf",             0x1D54D},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL V
    {"vopf",             0x1D567},  // MATHEMATICAL DOUBLE-STRUCK SMALL V
    {"vprop",            0x0221D},  // PROPORTIONAL TO
    {"vrtri",            0x022B3},  // CONTAINS AS NORMAL SUBGROUP
    {"Vscr",             0x1D4B1},  // MATHEMATICAL SCRIPT CAPITAL V
    {"vscr",             0x1D4CB},  // MATHEMATICAL SCRIPT SMALL V
    {"vsubne",           0x0228A, 0x0FE00},  // SUBSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
    {"vsubnE",           0x02ACB, 0x0FE00},  // SUBSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
    {"vsupne",           0x0228B, 0x0FE00},  // SUPERSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
    {"vsupnE",           0x02ACC, 0x0FE00},  // SUPERSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
    {"Vvdash",           0x022AA},  // TRIPLE VERTICAL BAR RIGHT TURNSTILE
    {"vzigzag",          0x0299A},  // VERTICAL ZIGZAG LINE
];

immutable NameId[] namesW =
[
    {"Wcirc",            0x00174},  // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    {"wcirc",            0x00175},  // LATIN SMALL LETTER W WITH CIRCUMFLEX
    {"wedbar",           0x02A5F},  // LOGICAL AND WITH UNDERBAR
    {"wedge",            0x02227},  // LOGICAL AND
    {"Wedge",            0x022C0},  // N-ARY LOGICAL AND
    {"wedgeq",           0x02259},  // ESTIMATES
    {"weierp",           0x02118},  // SCRIPT CAPITAL P
    {"Wfr",              0x1D51A},  // MATHEMATICAL FRAKTUR CAPITAL W
    {"wfr",              0x1D534},  // MATHEMATICAL FRAKTUR SMALL W
    {"Wopf",             0x1D54E},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL W
    {"wopf",             0x1D568},  // MATHEMATICAL DOUBLE-STRUCK SMALL W
    {"wp",               0x02118},  // SCRIPT CAPITAL P
    {"wr",               0x02240},  // WREATH PRODUCT
    {"wreath",           0x02240},  // WREATH PRODUCT
    {"Wscr",             0x1D4B2},  // MATHEMATICAL SCRIPT CAPITAL W
    {"wscr",             0x1D4CC},  // MATHEMATICAL SCRIPT SMALL W
];

immutable NameId[] namesX =
[
    {"xcap",             0x022C2},  // N-ARY INTERSECTION
    {"xcirc",            0x025EF},  // LARGE CIRCLE
    {"xcup",             0x022C3},  // N-ARY UNION
    {"xdtri",            0x025BD},  // WHITE DOWN-POINTING TRIANGLE
    {"Xfr",              0x1D51B},  // MATHEMATICAL FRAKTUR CAPITAL X
    {"xfr",              0x1D535},  // MATHEMATICAL FRAKTUR SMALL X
    {"Xgr",              0x0039E},  // GREEK CAPITAL LETTER XI
    {"xgr",              0x003BE},  // GREEK SMALL LETTER XI
    {"xharr",            0x027F7},  // LONG LEFT RIGHT ARROW
    {"xhArr",            0x027FA},  // LONG LEFT RIGHT DOUBLE ARROW
    {"Xi",               0x0039E},  // GREEK CAPITAL LETTER XI
    {"xi",               0x003BE},  // GREEK SMALL LETTER XI
    {"xlarr",            0x027F5},  // LONG LEFTWARDS ARROW
    {"xlArr",            0x027F8},  // LONG LEFTWARDS DOUBLE ARROW
    {"xmap",             0x027FC},  // LONG RIGHTWARDS ARROW FROM BAR
    {"xnis",             0x022FB},  // CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
    {"xodot",            0x02A00},  // N-ARY CIRCLED DOT OPERATOR
    {"Xopf",             0x1D54F},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL X
    {"xopf",             0x1D569},  // MATHEMATICAL DOUBLE-STRUCK SMALL X
    {"xoplus",           0x02A01},  // N-ARY CIRCLED PLUS OPERATOR
    {"xotime",           0x02A02},  // N-ARY CIRCLED TIMES OPERATOR
    {"xrarr",            0x027F6},  // LONG RIGHTWARDS ARROW
    {"xrArr",            0x027F9},  // LONG RIGHTWARDS DOUBLE ARROW
    {"Xscr",             0x1D4B3},  // MATHEMATICAL SCRIPT CAPITAL X
    {"xscr",             0x1D4CD},  // MATHEMATICAL SCRIPT SMALL X
    {"xsqcup",           0x02A06},  // N-ARY SQUARE UNION OPERATOR
    {"xuplus",           0x02A04},  // N-ARY UNION OPERATOR WITH PLUS
    {"xutri",            0x025B3},  // WHITE UP-POINTING TRIANGLE
    {"xvee",             0x022C1},  // N-ARY LOGICAL OR
    {"xwedge",           0x022C0},  // N-ARY LOGICAL AND
];

immutable NameId[] namesY =
[
    {"Yacute",           0x000DD},  // LATIN CAPITAL LETTER Y WITH ACUTE
    {"yacute",           0x000FD},  // LATIN SMALL LETTER Y WITH ACUTE
    {"YAcy",             0x0042F},  // CYRILLIC CAPITAL LETTER YA
    {"yacy",             0x0044F},  // CYRILLIC SMALL LETTER YA
    {"Ycirc",            0x00176},  // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    {"ycirc",            0x00177},  // LATIN SMALL LETTER Y WITH CIRCUMFLEX
    {"Ycy",              0x0042B},  // CYRILLIC CAPITAL LETTER YERU
    {"ycy",              0x0044B},  // CYRILLIC SMALL LETTER YERU
    {"yen",              0x000A5},  // YEN SIGN
    {"Yfr",              0x1D51C},  // MATHEMATICAL FRAKTUR CAPITAL Y
    {"yfr",              0x1D536},  // MATHEMATICAL FRAKTUR SMALL Y
    {"YIcy",             0x00407},  // CYRILLIC CAPITAL LETTER YI
    {"yicy",             0x00457},  // CYRILLIC SMALL LETTER YI
    {"Yopf",             0x1D550},  // MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
    {"yopf",             0x1D56A},  // MATHEMATICAL DOUBLE-STRUCK SMALL Y
    {"Yscr",             0x1D4B4},  // MATHEMATICAL SCRIPT CAPITAL Y
    {"yscr",             0x1D4CE},  // MATHEMATICAL SCRIPT SMALL Y
    {"YUcy",             0x0042E},  // CYRILLIC CAPITAL LETTER YU
    {"yucy",             0x0044E},  // CYRILLIC SMALL LETTER YU
    {"yuml",             0x000FF},  // LATIN SMALL LETTER Y WITH DIAERESIS
    {"Yuml",             0x00178},  // LATIN CAPITAL LETTER Y WITH DIAERESIS
];

immutable NameId[] namesZ =
[
    {"Zacute",           0x00179},  // LATIN CAPITAL LETTER Z WITH ACUTE
    {"zacute",           0x0017A},  // LATIN SMALL LETTER Z WITH ACUTE
    {"Zcaron",           0x0017D},  // LATIN CAPITAL LETTER Z WITH CARON
    {"zcaron",           0x0017E},  // LATIN SMALL LETTER Z WITH CARON
    {"Zcy",              0x00417},  // CYRILLIC CAPITAL LETTER ZE
    {"zcy",              0x00437},  // CYRILLIC SMALL LETTER ZE
    {"Zdot",             0x0017B},  // LATIN CAPITAL LETTER Z WITH DOT ABOVE
    {"zdot",             0x0017C},  // LATIN SMALL LETTER Z WITH DOT ABOVE
    {"zeetrf",           0x02128},  // BLACK-LETTER CAPITAL Z
    {"ZeroWidthSpace",   0x0200B},  // ZERO WIDTH SPACE
    {"Zeta",             0x00396},  // GREEK CAPITAL LETTER ZETA
    {"zeta",             0x003B6},  // GREEK SMALL LETTER ZETA
    {"Zfr",              0x02128},  // BLACK-LETTER CAPITAL Z
    {"zfr",              0x1D537},  // MATHEMATICAL FRAKTUR SMALL Z
    {"Zgr",              0x00396},  // GREEK CAPITAL LETTER ZETA
    {"zgr",              0x003B6},  // GREEK SMALL LETTER ZETA
    {"ZHcy",             0x00416},  // CYRILLIC CAPITAL LETTER ZHE
    {"zhcy",             0x00436},  // CYRILLIC SMALL LETTER ZHE
    {"zigrarr",          0x021DD},  // RIGHTWARDS SQUIGGLE ARROW
    {"Zopf",             0x02124},  // DOUBLE-STRUCK CAPITAL Z
    {"zopf",             0x1D56B},  // MATHEMATICAL DOUBLE-STRUCK SMALL Z
    {"Zscr",             0x1D4B5},  // MATHEMATICAL SCRIPT CAPITAL Z
    {"zscr",             0x1D4CF},  // MATHEMATICAL SCRIPT SMALL Z
    {"zwj",              0x0200D},  // ZERO WIDTH JOINER
    {"zwnj",             0x0200C},  // ZERO WIDTH NON-JOINER
];
