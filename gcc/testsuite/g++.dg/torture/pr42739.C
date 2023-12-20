/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */

struct s { ~s() { s(); } };

int f()
{
  M:				// { dg-warning "jump" }
    s o = s();			// { dg-message "does not destroy" }
    f();
    f();

  L:
    goto *(f() ? &&L : &&M);	// { dg-message "computed goto" }

    return 0;
}
