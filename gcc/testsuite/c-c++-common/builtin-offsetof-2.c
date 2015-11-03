// { dg-options "-Warray-bounds" }
// { dg-do compile }

// Test case exercising pr c/67882 - surprising offsetof result
//   on an invalid array member without diagnostic.

typedef struct A1 {
  char a1[1];
  char c;
} A1;

typedef struct A1_x_2 {
  char a1[1];
  char a[][2];
} A1_x_2;

typedef struct A1_1_x {
  char a1_1[1][1];
  char a[];
} A1_1_x;

typedef struct Ax_2_3 {
  int i;
  char a_x_2_3[][2][3];
} Ax_2_3;

typedef struct A1_1 {
  char a1_1[1][1];
  char c;
} A1_1;

typedef struct B {
  A1_1 a2_3[2][3];
  char a1_1[3][5];
  char a[];
} B;

// Structures with members that contain flexible array members are
// an extension accepted by GCC.
typedef struct C {
  A1_1_x a5_7 [5][7];
  int a;
} C;

// Structs with a "fake" flexible array member (a GCC extension).
typedef struct FA0 {
  int i;
  char a0 [0];
} FA0;

typedef struct FA1 {
  int i;
  char a1 [1];
} FA1;

typedef struct FA3 {
  int i;
  char a3 [3];
} FA3;

// A "fake" multidimensional flexible array member.
typedef struct FA5_7 {
  int i;
  char a5_7 [5][7];
} FA5_7;

static void test (void)
{
  // Verify that offsetof references to array elements past the end of
  // the array member are diagnosed.  As an extension, permit references
  // to the element just past-the-end of the array.

  int a[] = {
    __builtin_offsetof (A1, a1),                 // valid
    __builtin_offsetof (A1, a1 [0]),             // valid

    // The following expression is valid because it forms the equivalent
    // of an address pointing just past the last element of the array.
    __builtin_offsetof (A1, a1 [1]),             // valid

    __builtin_offsetof (A1, a1 [2]),             // { dg-warning "index" }

    __builtin_offsetof (A1_x_2, a1),             // valid
    __builtin_offsetof (A1_x_2, a1 [0]),         // valid
    __builtin_offsetof (A1_x_2, a1 [1]),         // valid
    __builtin_offsetof (A1_x_2, a1 [2]),         // { dg-warning "index" }

    __builtin_offsetof (A1_x_2, a),              // valid
    __builtin_offsetof (A1_x_2, a [0]),          // valid
    __builtin_offsetof (A1_x_2, a [1]),          // valid
    __builtin_offsetof (A1_x_2, a [99]),         // valid

    __builtin_offsetof (A1_x_2, a),              // valid
    __builtin_offsetof (A1_x_2, a [0][0]),       // valid
    __builtin_offsetof (A1_x_2, a [0][1]),       // valid

    // The following expression is valid because it forms the equivalent
    // of an address pointing just past the last element of the first
    // array.
    __builtin_offsetof (A1_x_2, a [0][2]),       // valid

    // Unlike the case above, this is invalid since it refers to an element
    // past one one just-past-the-end in A[][2].
    __builtin_offsetof (A1_x_2, a [0][3]),       // { dg-warning "index" }

    __builtin_offsetof (A1_x_2, a [1][0]),       // valid
    __builtin_offsetof (A1_x_2, a [1][1]),       // valid
    __builtin_offsetof (A1_x_2, a [1][2]),       // valid
    __builtin_offsetof (A1_x_2, a [99][0]),      // valid
    __builtin_offsetof (A1_x_2, a [99][1]),      // valid
    __builtin_offsetof (A1_x_2, a [99][2]),      // valid

    __builtin_offsetof (A1_1_x, a),              // valid
    __builtin_offsetof (A1_1_x, a [0]),          // valid
    __builtin_offsetof (A1_1_x, a [1]),          // valid
    __builtin_offsetof (A1_1_x, a [99]),         // valid

    __builtin_offsetof (A1_1_x, a1_1 [0][0]),    // valid
    __builtin_offsetof (A1_1_x, a1_1 [0][1]),    // valid
    __builtin_offsetof (A1_1_x, a1_1 [0][2]),    // { dg-warning "index" }
    __builtin_offsetof (A1_1_x, a1_1 [1][0]),    // { dg-warning "index" }
    __builtin_offsetof (A1_1_x, a1_1 [1][1]),    // { dg-warning "index" }

    __builtin_offsetof (Ax_2_3, a_x_2_3 [0][1][3]),  // valid
    __builtin_offsetof (Ax_2_3, a_x_2_3 [0][1][4]),  // { dg-warning "index" }
    __builtin_offsetof (Ax_2_3, a_x_2_3 [0][2]),     // valid
    __builtin_offsetof (Ax_2_3, a_x_2_3 [0][2][0]),  // { dg-warning "index" }

    __builtin_offsetof (B, a2_3 [0][0].c),           // valid
    __builtin_offsetof (B, a2_3 [0][0].a1_1 [0][0]), // valid
    __builtin_offsetof (B, a2_3 [1][3]),             // valid
    __builtin_offsetof (B, a2_3 [1][4]),             // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [0][0].a1_1 [0][1]), // valid
    __builtin_offsetof (B, a2_3 [0][0].a1_1 [0][2]), // { dg-warning "index" }

    __builtin_offsetof (B, a2_3 [0][0].a1_1 [1][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [0][0].a1_1 [1][1]), // { dg-warning "index" }

    __builtin_offsetof (B, a2_3 [1][2].a1_1 [0][0]), // valid

    // Forming an offset to the just-past-end element is valid.
    __builtin_offsetof (B, a2_3 [1][2].a1_1 [0][1]), // valid
    __builtin_offsetof (B, a2_3 [1][2].a1_1 [1][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [1][2].a1_1 [1][1]), // { dg-warning "index" }

    // Forming an offset to the just-past-end element is valid.
    __builtin_offsetof (B, a2_3 [1][3]),             // valid
    // ...but these are diagnosed because they dereference a just-past-the-end
    // element.
    __builtin_offsetof (B, a2_3 [1][3].a1_1 [0][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [1][3].a1_1 [0][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [1][3].a1_1 [0][1]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [1][3].a1_1 [1][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [1][3].a1_1 [1][1]), // { dg-warning "index" }

    // Analogous to the case above, these are both diagnosed because they
    // dereference just-past-the-end elements of the a2_3 array.
    __builtin_offsetof (B, a2_3 [1][3].c),       // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [1][3].c),       // { dg-warning "index" }

    // The following are all invalid because of the reference to a2_3[2].
    __builtin_offsetof (B, a2_3 [2][0].a1_1 [0][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [2][0].a1_1 [0][1]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [2][0].a1_1 [1][0]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [2][0].a1_1 [1][1]), // { dg-warning "index" }
    __builtin_offsetof (B, a2_3 [2][0].c),           // { dg-warning "index" }

    __builtin_offsetof (C, a5_7 [4][6]),
    __builtin_offsetof (C, a5_7 [4][6].a),
    __builtin_offsetof (C, a5_7 [4][6].a [0]),
    __builtin_offsetof (C, a5_7 [4][6].a [99]),

    __builtin_offsetof (C, a5_7 [4][7]),             // valid
    // Diagnose the following even though the object whose offset is
    // computed is a flexible array member.
    __builtin_offsetof (C, a5_7 [4][7].a),           // { dg-warning "index" }
    __builtin_offsetof (C, a5_7 [4][7].a [0]),       // { dg-warning "index" }
    __builtin_offsetof (C, a5_7 [4][7].a [99]),      // { dg-warning "index" }

    // Verify that no diagnostic is issued for offsetof expressions
    // involving structs where the array has a rank of 1 and is the last
    // member (e.g., those are treated as flexible array members).
    __builtin_offsetof (FA0, a0 [0]),
    __builtin_offsetof (FA0, a0 [1]),
    __builtin_offsetof (FA0, a0 [99]),

    __builtin_offsetof (FA1, a1 [0]),
    __builtin_offsetof (FA1, a1 [1]),
    __builtin_offsetof (FA1, a1 [99]),

    __builtin_offsetof (FA3, a3 [0]),
    __builtin_offsetof (FA3, a3 [3]),
    __builtin_offsetof (FA3, a3 [99]),

    __builtin_offsetof (FA5_7, a5_7 [0][0]),

    // Unlike one-dimensional arrays, verify that out-of-bounds references
    // to "fake" flexible arrays with rank of 2 and greater are diagnosed.

    // The following are valid because they compute the offset of just past
    // the end of each of the a5_7[0] and a5_7[1] arrays.
    __builtin_offsetof (FA5_7, a5_7 [0][7]),         // valid
    __builtin_offsetof (FA5_7, a5_7 [1][7]),         // valid

    // The following two are accepted as an extesion (because a5_7 is
    // treated as a flexible array member).
    __builtin_offsetof (FA5_7, a5_7 [5][0]),         // extension
    __builtin_offsetof (FA5_7, a5_7 [5][7]),         // extension

    // The following are invalid since in both cases they denote an element
    // that's beyond just-past-the-end of the array.
    __builtin_offsetof (FA5_7, a5_7 [0][8]),        // { dg-warning "index" }
    __builtin_offsetof (FA5_7, a5_7 [6][8])         // { dg-warning "index" }
  };

  (void)&a;
}
