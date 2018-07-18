/* Static declarations followed by extern are OK even if the extern
   declaration is initialized.  Bug 15360 from hozelda at
   yahoo.com.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

static int a;
static int a;
extern int a;
static int a;

static int b;
extern int b = 1; /* { dg-warning "initialized and declared" "extern init warning" } */
static int b;
static int b;

static int c; /* { dg-message "note: previous declaration" } */
int c; /* { dg-error "non-static" "correct error" } */

static int d; /* { dg-message "note: previous declaration" } */
int d = 1; /* { dg-error "non-static" "correct error" } */

void foo (void) { extern int e = 1; } /* { dg-error "has both" "extern init in function" } */
