/* Test for obscuring of @interfaces with local vars.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface View
@end

void foo(void)
{
        int View;	/* ok */
        View = 1;	/* ok */
	View *view;	/* { dg-error "`view' undeclared" } */
	/* { dg-error "is reported only once" "" { target *-*-* } 12 } */
	/* { dg-error "function it appears in" "" { target *-*-* } 12 } */
}

void bar(void)
{
	View *view;	/* ok */
	View = 1;	/* { dg-error "(parse|syntax) error" } */
}
