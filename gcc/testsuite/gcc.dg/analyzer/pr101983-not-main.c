/* { dg-additional-options "-Wno-analyzer-too-complex -fno-analyzer-call-summaries" } */

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

struct list {
	struct list* next;
	void *a;
};

void func(struct list **res)
{
	struct list *cur = NULL;
	do {
		struct list *n = malloc(sizeof(struct list));
		void *a = malloc(1);
		if (n == NULL || a == NULL) {
			if (n != NULL) free(n);
			if (a != NULL) free(a);
			break;
		}

		if (cur == NULL) {
			*res = cur = n;
		} else {
			cur->next = n;
			cur = n;
		}
		n->a = a;
	} while (true);
}

int not_main()
{
	struct list *res;
	func(&res);
} /* { dg-warning "leak of 'res'" "leak of res" } */
/* { dg-warning "leak of '<unknown>'" "leak of res->a" { target *-*-* } .-1 } */
/* TODO: we should emit 'res->a' rather than '<unknown>' here.  */
