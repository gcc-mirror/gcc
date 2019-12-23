/* PR c++/61339 - add mismatch between struct and class
   Test to verify that -Wredundant-tags is issued for references to class
   types that use the class-key even though they don't need to.
   { dg-do compile }
   { dg-options "-Wredundant-tags" } */

struct A;

extern A *pa;
extern struct A *pa;        // { dg-warning "redundant class-key 'struct' in reference to 'struct A'" }

extern A aa[];
extern struct A aa[];       // { dg-warning "redundant class-key 'struct' in reference to 'struct A'" }

void func (A*);
void func (struct A*);      // { dg-warning "redundant class-key 'struct' in reference to 'struct A'" }

int A;

extern struct A *pa;
extern struct A aa[];
void func (struct A*);


class B;

extern B *pb;
extern class B *pb;         // { dg-warning "redundant class-key 'class' in reference to 'class B'" }

extern B ab[];
extern class B ab[];        // { dg-warning "redundant class-key 'class' in reference to 'class B'" }

void func (B*);
void func (class B*);       // { dg-warning "redundant class-key 'class' in reference to 'class B'" }

int B;

extern class B *pb;
extern class B ab[];
void func (class B*);


enum C { c0 };

extern C *pc;
extern enum C *pc;          // { dg-warning "redundant enum-key 'enum' in reference to 'enum C'" }

extern C ac[];
extern enum C ac[];         // { dg-warning "redundant enum-key 'enum' in reference to 'enum C'" }

void func (C*);
void func (enum C*);        // { dg-warning "redundant enum-key 'enum' in reference to 'enum C'" }

int C;

extern enum C *pc;
extern enum C ac[];
void func (enum C*);


#if __cplusplus > 199711L

enum class D1 { d1 };
enum struct D2 { d2 };

#else

enum D1 { d1 };
enum D2 { d2 };

#endif

extern D1 *pd1;
extern D2 *pd2;
extern enum D1 *pd1;        // { dg-warning "redundant enum-key 'enum' in reference to 'enum class D1'" "C++ 11 and above" { target c++11 } }
                            // { dg-warning "redundant enum-key 'enum' in reference to 'enum D1'" "C++ 98" { target c++98_only } .-1 }

extern enum D2 *pd2;        // { dg-warning "redundant enum-key 'enum' in reference to 'enum class D2'" "C++ 11 and above" { target c++11 } }
                            // { dg-warning "redundant enum-key 'enum' in reference to 'enum D2'" "C++ 98" { target c++98_only } .-1 }

extern D1 ad1[];
extern D2 ad2[];

#if __cplusplus > 199711L
extern enum class D1 ad1[]; // { dg-warning "redundant enum-key 'enum class' in reference to 'enum class D1'" "C++ 11 and above" { target c++11 } }
                            // { dg-warning "elaborated-type-specifier for a scoped enum must not use the 'class' keyword" "C++ 11 and above" { target c++11 } .-1 }
/* The pretty printer cannot differentiate between enum class and enum struct
   because the C++ front-end doesn't encode it so allow for both in the text
   of the warning below.  */
extern enum struct D2 ad2[]; // { dg-warning "redundant enum-key 'enum struct' in reference to 'enum \(class|struct\) D2'" "C++ 11 and above" { target c++11 } }
                            // { dg-warning "elaborated-type-specifier for a scoped enum must not use the 'struct' keyword" "C++ 11 and above" { target c++11 } .-1 }
#else
extern enum D1 ad1[];       // { dg-warning "redundant enum-key 'enum' in reference to 'enum D1'" "C++ 98" { target c++98_only } }
#endif

void func (D1*);
void func (enum D1*);       // { dg-warning "redundant enum-key 'enum' in reference to 'enum " }

void func (D2*);
void func (enum D2*);       // { dg-warning "redundant enum-key 'enum' in reference to 'enum " }

int D1, D2;

extern enum D1 *pd1;
extern enum D1 ad1[];
void func (enum D1*);

extern enum D2 *pd2;
extern enum D2 ad2[];
void func (enum D2*);


union U;

extern U *pu;
extern union U *pu;         // { dg-warning "redundant class-key 'union' in reference to 'union U'" }

extern U au[];
extern union U au[];        // { dg-warning "redundant class-key 'union' in reference to 'union U'" }

void func (U*);
void func (union U*);       // { dg-warning "redundant class-key 'union' in reference to 'union U'" }

int U;

extern union U *pu;
extern union U au[];
void func (union U*);
