/* Test C23 deprecated attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

[[nodiscard]] int c1 (void); /* { dg-message "declared here" } */
[[__nodiscard__ ("some reason")]] int c2 (void); /* { dg-message "declared here" } */

struct [[nodiscard ("struct reason")]] s1 { int a; };
struct [[__nodiscard__]] s2 { long b; };
struct s1 cs1 (void); /* { dg-message "declared here" } */
struct s2 cs2 (void); /* { dg-message "declared here" } */
typedef struct s2 s2t;
s2t cs3 (void); /* { dg-message "declared here" } */

union [[nodiscard]] u1 { int a; long b; };
union [[nodiscard ("union reason")]] u2 { short c; float d; };
union u1 cu1 (void); /* { dg-message "declared here" } */
union u2 cu2 (void); /* { dg-message "declared here" } */

enum [[nodiscard]] e1 { E1 };
enum [[nodiscard ("enum reason")]] e2 { E2 };
enum e1 ce1 (void); /* { dg-message "declared here" } */
enum e2 ce2 (void); /* { dg-message "declared here" } */
enum e1 ce1a (void);
int i;

[[nodiscard]] void v (void); /* { dg-warning "void return type" } */

int ok (void);

void
f (void)
{
  c1 (); /* { dg-warning "ignoring return value" } */
  c2 (); /* { dg-warning "some reason" } */
  cs1 (); /* { dg-warning "struct reason" } */
  cs2 (); /* { dg-warning "ignoring return value of type" } */
  cs3 (); /* { dg-warning "ignoring return value of type" } */
  cu1 (); /* { dg-warning "ignoring return value of type" } */
  cu2 (); /* { dg-warning "union reason" } */
  ce1 (); /* { dg-warning "ignoring return value of type" } */
  ce2 (); /* { dg-warning "enum reason" } */
  ok ();
  c1 (), ok (); /* { dg-warning "ignoring return value" } */
  cs1 (), ok (); /* { dg-warning "struct reason" } */
  ok (), cu1 (); /* { dg-warning "ignoring return value" } */
  ok (), (ok (), (ok (), ce2 ())); /* { dg-warning "enum reason" } */
  (ok (), cu1 ()), ok (); /* { dg-warning "ignoring return value" } */
  v ();
  (i ? ce1 : ce1a) (); /* { dg-warning "ignoring return value of type" } */
  (void) c1 ();
  (void) c2 ();
  (void) cs1 ();
  (void) cs2 ();
  (void) cs3 ();
  (void) cu1 ();
  (void) cu2 ();
  (void) ce1 ();
  (void) ce2 ();
  (void) (ok (), cu1 ());
  (void) (i ? ce1 : ce1a) ();
}
