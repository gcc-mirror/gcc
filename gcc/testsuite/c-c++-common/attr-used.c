/* { dg-do compile } */
/* { dg-options "-O3" } */

static void function_declaration_before(void) __attribute__((__used__));

static void function_declaration_before(void) {}

static void function_declaration_after(void) {}

static void function_declaration_after(void) __attribute__((__used__));

/* { dg-final { scan-assembler "function_declaration_before" } } */
/* { dg-final { scan-assembler "function_declaration_after" } } */
/* { dg-final { scan-assembler-not "\.text.*,\"axR\"" { target R_flag_in_section } } } */
