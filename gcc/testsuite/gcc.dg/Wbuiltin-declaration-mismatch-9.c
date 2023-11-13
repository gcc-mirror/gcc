/* PR c/88886 - ice in get_constant, at c-family/c-format.c:292
   { dg-do compile }
   { dg-options "-fpermissive -Wall" } */

int sscanf (long, unsigned[], ...);   /* { dg-warning "conflicting types for built-in function .sscanf.; expected .int\\\(const char \\\*, const char \\\*, ...\\\)." } */

void a (void)
{
  sscanf (0,
	  ""        /* { dg-warning "passing argument 2 of .sscanf. from incompatible pointer type" } */
	  );
}

/* The invalid scanf call may also trigger:
   { dg-prune-output "accessing 4 bytes in a region of size 1" }
   { dg-prune-output "accessing 2 bytes in a region of size 1" } */
