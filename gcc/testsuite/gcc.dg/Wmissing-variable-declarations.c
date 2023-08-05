/* { dg-do compile } */
/* { dg-options "-Wmissing-variable-declarations" } */

int b0; /* { dg-warning "no previous declaration for 'b0'" } */

int b1 = 1; /* { dg-warning "no previous declaration for 'b1'" } */

int b2; /* { dg-warning "no previous declaration for 'b2'" } */
int b2 = 2; 

struct {
    int g0;
} b3; /* { dg-warning "no previous declaration for 'b3'" } */

int b4; /* { dg-warning "no previous declaration for 'b4'" } */
int b4 = 3;
extern int b4;

static int g1;

void g2(void);

extern int g3;
int g3;
int g3 = 4;

struct g4 {
    int g5;
};

void g6(void) {
    int g7;
}
