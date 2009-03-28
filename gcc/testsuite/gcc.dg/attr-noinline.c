/* { dg-do compile } */
/* { dg-options "-O2 -finline-functions" } */

extern int t();

static inline void __attribute__((__noinline__)) function_definition(void) {t();} /* { dg-warning "inline function \[^\n\]* given attribute noinline" "" } */

static inline void __attribute__((__noinline__)) function_declaration_both_before(void); /* { dg-warning "inline function \[^\n\]* given attribute noinline" "" } */

static void function_declaration_both_before(void) {t();}

static void function_declaration_both_after(void);

static inline void __attribute__((__noinline__)) function_declaration_both_after(void); /* { dg-warning "(inline function \[^\n\]* given attribute noinline|declared inline after its definition)" "" } */

static void function_declaration_both_after(void) {t();}

static void function_declaration_noinline_before(void) __attribute__((__noinline__)); /* { dg-message "note: previous declaration" "" } */

static inline void function_declaration_noinline_before(void) {t();} /* { dg-warning "follows declaration with attribute noinline" "" } */

static inline void function_declaration_noinline_after(void) {t();} /* { dg-message "note: previous definition" "" } */

static void function_declaration_noinline_after(void) __attribute__((__noinline__)); /* { dg-warning "follows inline declaration" "" } */

static inline void function_declaration_inline_before(void); /* { dg-message "note: previous declaration" "" } */

static void __attribute__((__noinline__)) function_declaration_inline_before(void) {t();} /* { dg-warning "follows inline declaration" "" } */

static inline void function_declaration_inline_noinline_before(void); /* { dg-message "note: previous declaration" "" } */

static void function_declaration_inline_noinline_before(void) __attribute__((__noinline__)); /* { dg-warning "follows inline declaration" "" } */

static void function_declaration_inline_noinline_before(void) {t();}

static inline void function_declaration_inline_noinline_after(void);

static void function_declaration_inline_noinline_after(void) {t();} /* { dg-message "note: previous definition" "" } */

static void function_declaration_inline_noinline_after(void) __attribute__((__noinline__)); /* { dg-warning "follows inline declaration" "" } */

static void function_declaration_noinline_inline_before(void) __attribute__((__noinline__)); /* { dg-message "note: previous declaration" "" } */

static inline void function_declaration_noinline_inline_before(void); /* { dg-warning "follows declaration with attribute noinline" "" } */

static void function_declaration_noinline_inline_before(void) {t();}

void f () {
  function_definition ();
  function_declaration_both_before ();
  function_declaration_both_after ();
  function_declaration_noinline_before ();
  function_declaration_noinline_after ();
  function_declaration_inline_before ();
  function_declaration_inline_noinline_before ();
  function_declaration_inline_noinline_after ();
  function_declaration_noinline_inline_before ();
}

/* { dg-final { scan-assembler "function_definition" } } */
/* { dg-final { scan-assembler "function_declaration_both_before" } } */
/* { dg-final { scan-assembler "function_declaration_both_after" } } */
/* { dg-final { scan-assembler "function_declaration_noinline_before" } } */
/* { dg-final { scan-assembler "function_declaration_noinline_after" } } */
/* { dg-final { scan-assembler "function_declaration_inline_before" } } */
/* { dg-final { scan-assembler "function_declaration_inline_noinline_before" } } */
/* { dg-final { scan-assembler "function_declaration_inline_noinline_after" } } */
/* { dg-final { scan-assembler "function_declaration_noinline_inline_before" } } */
