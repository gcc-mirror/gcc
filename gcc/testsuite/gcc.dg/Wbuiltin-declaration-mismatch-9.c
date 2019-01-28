/* PR c/88886 - ice in get_constant, at c-family/c-format.c:292
   { dg-do compile }
   { dg-options "-Wall" } */

int sscanf (long, unsigned[], ...);   /* { dg-warning "conflicting types for built-in function .sscanf.; expected .int\\\(const char \\\*, const char \\\*, ...\\\)." } */

void a (void)
{
  sscanf (0,
	  ""        /* { dg-warning "passing argument 2 of .sscanf. from incompatible pointer type" } */
	  );
}
