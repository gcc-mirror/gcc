/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify that -Warray-parameter diagnoses mismatches in array (and
   pointer) arrguments between redeclarations of the same function.
   Also verify that the array/pointer argument form in a mismatched
   redeclaration doesn't override the form in the initial declaration.
   { dg-do compile }
   { dg-options "-Wall -Warray-parameter -Wno-vla-parameter" } */

/* Redclarations with the same or equivalent array form should not
   be dianosed.  T[0] is diagnosed by -Wpedantic for being invalid
   C so there's little point in also warning for the difference in
   array form.  */
void f1vpp (void**);
void f1vpp (void*[]);
void f1vpp (void*[0]);

void f1ia_ (int[]);
void f1ia_ (int[]);
void f1ia_ (int[0]);
/* Verify the unused attribute still has an effect.  */
void f1ia_ (int a[0] __attribute__ ((unused))) { }
void f1ia_ (int[]);

void f1ia_p (int[]);
void f1ia_p (int*);
void f1ia_p (int *p  __attribute__ ((unused))) { }
void f1ia_p (int[]);

void f1p_ia (const int*);
void f1p_ia (const int[]);
void f1p_ia (const int *p __attribute__ ((unused))) { }
void f1p_ia (const int[]);

void f1ia1 (int[1]);
void f1ia1 (int[1]);
void f1ia1 (int[2 - 1]);

void f1ias2 (int[static 2]);
void f1ias2 (int[static 2]);
void f1ias2 (int[static 1 + 1]);
void f1ias2 (int a[static 3 - 1]) { (void)&a; }

void f1ipa_ (int*[]);
void f1ipa_ (int*[]);
void f1ipa_ (int*[0]);

void f1ia1_x (int[1]);          // { dg-message "previously declared as 'int\\\[1]'" }
void f1ia1_x (int[]);           // { dg-warning "argument 1 of type 'int\\\[]' with mismatched bound" }
void f1ia1_x (int[]);           // { dg-warning "argument 1 of type 'int\\\[]' with mismatched bound" }
void f1ia1_x (int[1]);
void f1ia1_x (int[2]);          // { dg-warning "argument 1 of type 'int\\\[2]' with mismatched bound" }
void f1ia1_x (int[1]);
void f1ia1_x (int[3]);          // { dg-warning "argument 1 of type 'int\\\[3]' with mismatched bound" }
void f1ia1_x (int a[1] __attribute__ ((unused))) { }


void f1ias2_s3 (int[static 2]); // { dg-message "previously declared as 'int\\\[static 2]'" }
void f1ias2_s3 (int[static 3]); // { dg-warning "argument 1 of type 'int\\\[static 3]' with mismatched bound" }
/* Verify the unused attribute still has an effect and doesn't interfere
   with the warning.  */
void f1ias2_s3 (int a[static 3] __attribute__ ((unused))) { }  // { dg-warning "argument 1 of type 'int\\\[static 3]' with mismatched bound" }


/* Ordinary T[N] and T[static N] forms are both effectively treated
   the same but strictly have different meanings so they are diagnosed.
   It might be worth splitting the warning into two levels and having
   only the higher level treat the ordinary form as T[static N].  */

void f1ia3_s4 (int[3]);         // { dg-message "previously declared as 'int\\\[3]'" }
void f1ia3_s4 (int[static 4]);  // { dg-warning "argument 1 of type 'int\\\[static 4]' with mismatched bound" }
void f1ia3_s4 (int[3]);


void f1ias4_5 (int[static 4]);  // { dg-message "previously declared as 'int\\\[static 4]'" }
void f1ias4_5 (int[5]);         // { dg-warning "argument 1 of type 'int\\\[5]' with mismatched bound" }
void f1ias4_5 (int[static 4]);


void f1ia_1 (int[]);            // { dg-message "previously declared as 'int\\\[]'" }
void f1ia_1 (int[1]);           // { dg-warning "argument 1 of type 'int\\\[1]' with mismatched bound" }
void f1ia_1 (int[]);


void f1ca_ (char[]);            // { dg-message "previously declared as 'char\\\[]'" }
void f1ca_ (char[2]);           // { dg-warning "argument 1 of type 'char\\\[2]' with mismatched bound" }
void f1ca_ (char[]);


void f1csp (const short*);      // { dg-message "previously declared as 'const short int ?\\\*'" }
void f1csp (const short[3]);    // { dg-warning "argument 1 of type 'const short int\\\[3]' with mismatched bound" }
void f1csp (const short*);


void f1ia2 (int[2]);            // { dg-message "previously declared as 'int\\\[2]'" }
void f1ia2 (int[1]);            // { dg-warning "argument 1 of type 'int\\\[1]' with mismatched bound" }
void f1ia2 (int[2]);


void f1cvla2 (const volatile long[3]);  // { dg-message "previously declared as 'const volatile long int\\\[3]'" }
void f1cvla2 (const volatile long[2]);  // { dg-warning "argument 1 of type 'const volatile long int\\\[2]' with mismatched bound" }
void f1cvla2 (const volatile long[3]);
void f1cvla2 (const volatile long[restrict 4]); // { dg-warning "argument 1 of type 'const volatile long int\\\[restrict 4]' with mismatched bound" }


void f1afa4 (_Atomic float[3]);         // { dg-message "previously declared as an array '_Atomic float ?\\\[3]'" }
void f1afa4 (_Atomic float*);           // { dg-warning "argument 1 of type '_Atomic float ?\\\*' declared as a pointer" }
void f1afa4 (_Atomic float[3]);

void f1ipa1_a2 (int*[1]);       // { dg-message "previously declared as 'int \\\*\\\[1]'" }
void f1ipa1_a2 (int*[2]);       // { dg-warning "argument 1 of type 'int \\\*\\\[2]' with mismatched bound" }
void f1ipa1_a2 (int*[1]);


typedef int IAx[];
typedef int IA1[1];
typedef int IA2[2];
typedef int IA3[3];

// The message should differentiate between the [] form and *.
void f1IAx_A1 (IAx);            // { dg-message "previously declared as 'int\\\[]'" "pr?????" { xfail *-*-* } }
                                // { dg-message "previously declared as 'int *\\\*'" "note" { target *-*-* } .-1 }
void f1IAx_A1 (IA1);            // { dg-message "argument 1 of type 'int\\\[1]' with mismatched bound" }

void f1IA1_A2 (IA1);            // { dg-message "previously declared as 'int\\\[1]'" }
void f1IA1_A2 (IA2);            // { dg-warning "argument 1 of type 'int\\\[2]' with mismatched bound" }
void f1IA1_A2 (IA1);
void f1IA1_A2 (int[2]);         // { dg-warning "argument 1 of type 'int\\\[2]' with mismatched bound" }


void f1IA1_A3 (IA1 ia1);        // { dg-message "previously declared as 'int\\\[1]'" }
void f1IA1_A3 (IA3 ia3);        // { dg-warning "argument 1 of type 'int\\\[3]' with mismatched bound" }
void f1IA1_A3 (IA1 ia1);


void f1IA2_A3 (IA2 a);          // { dg-message "previously declared as 'int\\\[2]'" }
void f1IA2_A3 (IA3 a);          // { dg-warning "argument 1 of type 'int\\\[3]' with mismatched bound" }
void f1IA2_A3 (IA2 a);


// Verify multiple array arguments.

void f2a2_a3_3_3 (int[2], int[3]);  // { dg-message "previously declared as 'int\\\[2]'" }
void f2a2_a3_3_3 (int[2], int[3]);
void f2a2_a3_3_3 (int[3], int[3]);  // { dg-warning "argument 1 of type 'int\\\[3]' with mismatched bound" }


void f2a2_a3_2_4 (int[2], int[3]);  // { dg-message "previously declared as 'int\\\[3]'" }
void f2a2_a3_2_4 (int[2], int[4]);  // { dg-warning "argument 2 of type 'int\\\[4]' with mismatched bound" }


/* Verify that pointers to arrays and arrays of arrays are differentiated
   the same way as pointers and arrays of other types.  */
typedef IA1 *PA1;

void fpia1 (IA1*);              // { dg-message "previously declared as 'int ?\\(\\\*\\)\\\[1]'" }
void fpia1 (IA1[1]);            // { dg-warning "argument 1 of type 'int\\\[1]\\\[1]' with mismatched bound" }
void fpia1 (PA1);
void fpia1 (int(*)[1]);
void fpia1 (int[][1]);

void f1vpa1 (void*[][1]);
void f1vpa1 (void*[0][1]);

/* Verify arrays of pointers.  */
void vaip1 (int (*[3]));       // { dg-message "previously declared as 'int *\\\*\\\[3]'" }
void vaip1 (int (*[5]));       // { dg-warning "argument 1 of type 'int *\\\*\\\[5]' with mismatched bound" }
void vaip1 (int (*[3]));
void vaip1 (int (*[]));        // { dg-warning "argument 1 of type 'int *\\\*\\\[]' with mismatched bound" }
void vaip1 (int (*[3]));

/* Verify that attributes with arrays don't cause unwanted warnings and
   don't suppress intended ones.  */

#define ALIGN(N)__attribute__ ((aligned (__alignof__ (char[N]))))

void fatipa2 (int (* ALIGN (3)[2]));  // { dg-message "previously declared as 'int \\\*\\\[2]'" }
void fatipa2 (int (* ALIGN (4)[2]));
void fatipa2 (int (* ALIGN (5)[2]));
void fatipa2 (int (* ALIGN (7)[3]));  // { dg-warning "argument 1 of type 'int \\\*\\\[3]' with mismatched bound" }

void fatiap (int (* ALIGN (3))[2]);
void fatiap (int (* ALIGN (5))[2]);


void fatipa3 (int (* ALIGN (1) (* ALIGN (2))[3]));
void fatipa3 (int (* ALIGN (1) (* ALIGN (2))[3]));
