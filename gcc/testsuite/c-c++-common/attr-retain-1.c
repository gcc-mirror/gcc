/* { dg-do compile { target R_flag_in_section } } */
/* { dg-options "-O3" } */

static void function_declaration_before(void)
  __attribute__((__used__, __retain__));

static void function_declaration_before(void) {}

static void function_declaration_after(void) {}

static void function_declaration_after(void)
  __attribute__((__used__, __retain__));

/* { dg-final { scan-assembler "function_declaration_before" } } */
/* { dg-final { scan-assembler "function_declaration_after" } } */
/* { dg-final { scan-assembler "\.text.*,\"axR\"" { target R_flag_in_section } } } */
