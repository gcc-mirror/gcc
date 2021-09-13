/* PR c/97882 - Segmentation Fault on improper redeclaration of function
   { dg-do compile }
   { dg-options "" } */

// Check pointer declaration incompatibiliies.

extern enum E e_u;      // { dg-message "note: previous declaration of 'e_u' with type 'enum E'" "note" }
unsigned e_u;           // { dg-error "conflicting types for 'e_u'; have 'unsigned int'" }


extern enum E *p;       // { dg-message "note: previous declaration of 'p' with type 'enum E \\*'" "note" }
unsigned *p;            // { dg-error "conflicting types for 'p'; have 'unsigned int \\*'" }

extern enum E **p2;     // { dg-message "note: previous declaration of 'p2' with type 'enum E \\*\\*'" "note" }
unsigned **p2;          // { dg-error "conflicting types for 'p2'; have 'unsigned int \\*\\*'" }

extern enum E ***p3;    // { dg-message "note: previous declaration of 'p3' with type 'enum E \\*\\*\\*'" "note" }
unsigned ***p3;         // { dg-error "conflicting types for 'p3'; have 'unsigned int \\*\\*\\*'" }

extern enum F *q;       // { dg-message "note: previous declaration of 'q' with type 'enum F \\*'" "note" }
int *q;                 // { dg-error "conflicting types for 'q'; have 'int \\*'" }

extern enum E* r[];     // { dg-message "note: previous declaration of 'r' with type 'enum E \\*\\\[]'" "note" }
extern unsigned *r[1];  // { dg-error "conflicting types for 'r'; have 'unsigned int \\*\\\[1]'" }

extern enum E **r2[];   // { dg-message "note: previous declaration of 'r2' with type 'enum E \\*\\*\\\[]'" "note" }
extern unsigned **r2[2];// { dg-error "conflicting types for 'r2'; have 'unsigned int \\*\\*\\\[2]'" }


typedef enum E* EPAx[];
typedef unsigned* UPAx[];

extern EPAx* peax;      //  { dg-message "note: previous declaration of 'peax' with type 'enum E \\* \\(\\*\\)\\\[]'" "note" }
extern UPAx* peax;      // { dg-error "conflicting types for 'peax'; have 'unsigned int \\* \\(\\*\\)\\\[]'" }


/* Check incompatibilities in the return type in a redeclaration
   of a function without a prototye.  */

/* Verify the following isn't rejected.  */
void f_v ();
void f_v (void);

enum E fE_u ();        // { dg-message "previous declaration of 'fE_u' with type 'enum E\\(\\)'" "note" }
unsigned fE_u ();      // { dg-error "conflicting types for 'fE_u'; have 'unsigned int\\(\\)'" }

enum E* fpE_u ();      // { dg-message "previous declaration of 'fpE_u' with type 'enum E \\*\\(\\)'" "note" }
unsigned* fpE_u ();    // { dg-error "conflicting types for 'fpE_u'; have 'unsigned int \\*\\(\\)'" }

enum E** fppE_u ();     // { dg-message "previous declaration of 'fppE_u' with type 'enum E \\*\\*\\(\\)'" "note" }
unsigned** fppE_u ();   // { dg-error "conflicting types for 'fppE_u'; have 'unsigned int \\*\\*\\(\\)'" }

enum E** fppE_u ();     // { dg-message "previous declaration of 'fppE_u' with type 'enum E \\*\\*\\(\\)'" "note" }
unsigned** fppE_u ();   // { dg-error "conflicting types for 'fppE_u'; have 'unsigned int \\*\\*\\(\\)'" }

enum E gE_u ();        // { dg-message "previous declaration of 'gE_u' with type 'enum E\\(\\)'" "note" }
unsigned gE_u ()       // { dg-error "conflicting types for 'gE_u'; have 'unsigned int\\(\\)'" }
{ return 0; }

enum E** gppE_u ();    // { dg-message "previous declaration of 'gppE_u' with type 'enum E \\*\\*\\(\\)'" "note" }
unsigned** gppE_u ()   // { dg-error "conflicting types for 'gppE_u'; have 'unsigned int \\*\\*\\(\\)'" }
{ return 0; }

unsigned fu_E ();      // { dg-message "previous declaration of 'fu_E' with type 'unsigned int\\(\\)'" "note" }
enum E fu_E ();        // { dg-error "conflicting types for 'fu_E'; have 'enum E\\(\\)'" }

unsigned gu_E ();      // { dg-message "previous declaration of 'gu_E' with type 'unsigned int\\(\\)'" "note" }
enum E gu_E () { }     // { dg-error "conflicting types for 'gu_E'" }
                       // { dg-error "incomplete type" "return type" { target *-*-* } .-1 }

typedef enum E FE_ ();
typedef unsigned Fuv (void);

FE_* fpF_u ();         // // { dg-message "previous declaration of 'fpF_u' with type 'enum E \\(\\*\\(\\)\\)\\(\\)'" "note" }
Fuv* fpF_u ();         // { dg-error "conflicting types for 'fpF_u'; have 'unsigned int \\(\\*\\(\\)\\)\\(void\\)'" }


typedef void Fv_ ();
typedef void Fvv (void);

/* Verify the following isn't rejected.  */
Fv_* f ();
Fvv* f ();


/* Check incompatibilities in argument types of a function redeclaration.  */

void fvE_u (enum E);    // { dg-message "note: previous declaration of 'fvE_u' with type 'void\\(enum E\\)'" "note" }
void fvE_u (unsigned);  // { dg-error "conflicting types for 'fvE_u'; have 'void\\(unsigned int\\)'" }

void fviE_u (int, enum E);    // { dg-message "note: previous declaration of 'fviE_u' with type 'void\\(int, *enum E\\)'" "note" }
void fviE_u (int, unsigned);  // { dg-error "conflicting types for 'fviE_u'; have 'void\\(int, *unsigned int\\)'" }

void fvE_el (enum E, ...);    // { dg-message "note: previous declaration of 'fvE_el' with type 'void\\(enum E, \\.\\.\\.\\)'" "note" }
void fvE_el (unsigned, ...);  // { dg-error "conflicting types for 'fvE_el'; have 'void\\(unsigned int, \\.\\.\\.\\)'" }


/* Check incompatibilities in the return type in a redeclaration
   of a nested function without a prototye.  */

void f1 (void)
{
  enum G f11 ();        // { dg-message "note: previous declaration of 'f11' with type 'enum G\\(\\)'" "note" }
  unsigned f11 () { }   // { dg-error "conflicting types for 'f11'; have 'unsigned int\\(\\)'" }
}


void f2 (void)
{
  const enum G f21 ();  // { dg-message "note: previous declaration of 'f21' with type 'enum G\\(\\)'" "note" }
  unsigned f21 () { }   // { dg-error "conflicting types for 'f21'; have 'unsigned int\\(\\)'" }
}


void f3 (void)
{
  enum G f31 ();        // { dg-message "note: previous declaration of 'f31' with type 'enum G\\(\\)'" "note" }
  const unsigned f31 () { }   // { dg-error "conflicting types for 'f31'; have 'unsigned int\\(\\)'" }
}


void f4 (void)
{
  auto enum G f31 ();         // { dg-message "note: previous declaration of 'f31' with type 'enum G\\(\\)'" "note" }
  const unsigned f31 () { }   // { dg-error "conflicting types for 'f31'; have 'unsigned int\\(\\)'" }
}


void f5 (void)
{
  enum G* f51 ();       // { dg-message "note: previous declaration of 'f51' with type 'enum G \\*\\(\\)'" "note" }
  int* f51 () { }       // { dg-error "conflicting types for 'f51'; have 'int \\*\\(\\)'" }
}


void f6 (void)
{
  enum G;
  void f61 (enum G);    // { dg-message "note: previous declaration of 'f61' with type 'void\\(enum G\\)'" "note" }
  void f61 (unsigned)   // { dg-error "conflicting types for 'f61'; have 'void\\(unsigned int\\)'" }
  { }
}

// { dg-prune-output "nested function '\[^\n\r ]+' declared but never defined" }
