/* PR c/117178 */
/* { dg-do compile } */
/* { dg-options "-Wunterminated-string-initialization" } */

const char a[][4] __attribute__((nonstring)) = {
  "ABCD",
  "EFGH",
  "IJK",
  "LMNO"
};
const char b[][2][2] __attribute__((nonstring)) = {
  { "PQ",
    "R" },
  { "S",
    "TU" }
};
struct S { int c; char d[3][5] __attribute__((nonstring)); int e; } f = {
  0,
  { "abcde",
    "defg",
    "hijkl" },
  1
};
struct T { int g; char h[3][2][2] __attribute__((nonstring)); int i; } j = {
  0,
  { { "m",
      "no" },
    { "pq",
      "r" },
    { "s",
      "tu" } },
  1
};
const char k[][4] = {
  "ABCD",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(5 chars into 4 available\\\)" } */
  "EFGH",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(5 chars into 4 available\\\)" } */
  "IJK",
  "LMNO"	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(5 chars into 4 available\\\)" } */
};
const char l[][2][2] = {
  { "PQ",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
    "R" },
  { "S",
    "TU" }	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
};
struct U { int m; char n[3][5]; int o; } p = {
  0,
  { "abcde",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(6 chars into 5 available\\\)" } */
    "defg",
    "hijkl" },	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(6 chars into 5 available\\\)" } */
  1
};
struct V { int q; char r[3][2][2]; int s; } t = {
  0,
  { { "m",
      "no" },	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
    { "pq",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
      "r" },
    { "s",
      "tu" } },	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
  1
};
const char u[][4] __attribute__((nonstring)) = {
  [3] = "ABCD",
  [1] = "EFGH",
  "IJK",
  [0] = "LMNO"
};
const char v[][2][2] __attribute__((nonstring)) = {
  [1][1] = "PQ",
  [0][1] = "R",
  [0][0] = "S",
  [1][0] = "TU"
};
struct S w = {
  0,
  { [2] = "abcde",
    [0] = "defg",
    [1] = "hijkl" },
  1
};
struct T x = {
  .i = 0,
  .h = { [2][1] = "m",
	 [1][1] = "no",
	 [0][0] = "pq" },
  .h[1][0] = "r",
  .h[2][0] = "s",
  .g = 1,
  .h[0][1] = "tu"
};
const char y[][4] = {
  [3] = "ABCD",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(5 chars into 4 available\\\)" } */
  [1] = "EFGH",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(5 chars into 4 available\\\)" } */
  "IJK",
  [0] = "LMNO"	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(5 chars into 4 available\\\)" } */
};
const char z[][2][2] = {
  [1][1] = "PQ",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
  [0][1] = "R",
  [0][0] = "S",
  [1][0] = "TU"		/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
};
struct U aa = {
  0,
  { [2] = "abcde",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(6 chars into 5 available\\\)" } */
    [0] = "defg",
    [1] = "hijkl" },	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(6 chars into 5 available\\\)" } */
  1
};
struct V ab = {
  .s = 0,
  .r = { [2][1] = "m",
	 [1][1] = "no",	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
	 [0][0] = "pq" }, /* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
  .r[1][0] = "r",
  .r[2][0] = "s",
  .q = 1,
  .r[0][1] = "tu"	/* { dg-warning "initializer-string for array of 'char' truncates NUL terminator but destination lacks 'nonstring' attribute \\\(3 chars into 2 available\\\)" } */
};
