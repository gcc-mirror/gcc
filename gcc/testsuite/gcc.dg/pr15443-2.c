/* { dg-do compile } */

struct foo {
	int bar;
};

typedef struct foo* bar;
void f () __attribute__ ((__malloc__)); /* { dg-warning "ignored" } */
int g () __attribute__ ((__malloc__)); /* { dg-warning "ignored" } */
int* h () __attribute__ ((__malloc__));
void* i () __attribute__ ((__malloc__));

struct foo j () __attribute__ ((__malloc__)); /* { dg-warning "ignored" } */
struct foo* k () __attribute__ ((__malloc__));
bar l () __attribute__((malloc));
