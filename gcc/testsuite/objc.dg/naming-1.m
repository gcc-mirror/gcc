/* Test for obscuring of @interfaces with local vars.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface View
@end

void foo(void)
{
        int View;	/* ok */
        View = 1;	/* ok */
	View *view;	/* { dg-error "undeclared|only once|it appears" } */
}

void bar(void)
{
	View *view;	/* ok */
	View = 1;	/* { dg-error "parse error|syntax error|expected" } */
}
