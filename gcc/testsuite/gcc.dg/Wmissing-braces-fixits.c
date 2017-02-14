/* { dg-options "-Wmissing-braces -fdiagnostics-show-caret" } */

struct sf2 { int i; int j; };
struct sf3 { int i; int j; int k; };
struct sa2 { int arr[2]; };
struct sa3 { int arr[3]; };

int arr_12[12] = \
  { 0, 1, 2, 3, 4, 5,
    6, 7, 8, 9, 10, 11};

int arr_12_1[12][1] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {} {} {} {} {} {}
     6, 7, 8, 9, 10, 11};
     {} {} {} {} { } { }
     { dg-end-multiline-output "" } */

int arr_1_12[1][12] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     6, 7, 8, 9, 10, 11};
                       }
     { dg-end-multiline-output "" } */

int arr_2_6[2][6] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {               }
     6, 7, 8, 9, 10, 11};
     {                 }
     { dg-end-multiline-output "" } */

int arr_2_2_3[2][2][3] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     {      } {      }
                     }
     6, 7, 8, 9, 10, 11};
     {
     {      } {        }
                       }
     { dg-end-multiline-output "" } */

int arr_2_3_2[2][3][2] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     {   } {   } {   }
                     }
     6, 7, 8, 9, 10, 11};
     {
     {   } {   } {     }
                       }
     { dg-end-multiline-output "" } */

int arr_6_2[6][2] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {   } {   } {   }
     6, 7, 8, 9, 10, 11};
     {   } {   } {     }
     { dg-end-multiline-output "" } */

int arr_3_2_2[3][2][2] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     {   } {   }
               } {
                 {   }
     6, 7, 8, 9, 10, 11};
     {   }
         } {
           {   } {     }
                       }
     { dg-end-multiline-output "" } */

int arr_3_4[3][4] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {         } {
     6, 7, 8, 9, 10, 11};
         } {           }
     { dg-end-multiline-output "" } */

int arr_4_3[4][3] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {      } {      }
     6, 7, 8, 9, 10, 11};
     {      } {        }
     { dg-end-multiline-output "" } */

int arr_2_1_6[2][1][6] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     {               }
                     }
     6, 7, 8, 9, 10, 11};
     {
     {                 }
                       }
     { dg-end-multiline-output "" } */

struct sf2 arr_6_sf2[6] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {   } {   } {   }
     6, 7, 8, 9, 10, 11};
     {   } {   } {     }
     { dg-end-multiline-output "" } */

struct sf3 arr_4_sf3[4] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {      } {      }
     6, 7, 8, 9, 10, 11};
     {      } {        }
     { dg-end-multiline-output "" } */

struct sa2 arr_6_sa2[6] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     {   }
         } {
           {   }
               } {
                 {   }
                     }
     6, 7, 8, 9, 10, 11};
     {
     {   }
         } {
           {   }
               } {
                 {     }
                       }
     { dg-end-multiline-output "" } */

struct sa3 arr_4_sa3[4] = \
  { 0, 1, 2, 3, 4, 5, /* { dg-warning "missing braces around initializer" } */
    6, 7, 8, 9, 10, 11};
  /* { dg-begin-multiline-output "" }
   { 0, 1, 2, 3, 4, 5,
   ^
     {
     {      }
            } {
              {      }
                     }
     6, 7, 8, 9, 10, 11};
     {
     {      }
            } {
              {        }
                       }
     { dg-end-multiline-output "" } */
