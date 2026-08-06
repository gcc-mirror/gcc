// P3658R1
// { dg-do compile }
// { dg-options "-pedantic-errors" }

int ²x = 42;	// { dg-error "extended character ² is not valid at the start of an identifier" }
int ³x = 42;	// { dg-error "extended character ³ is not valid at the start of an identifier" }
int ¹x = 42;	// { dg-error "extended character ¹ is not valid at the start of an identifier" }
int ⁰x = 42;	// { dg-error "extended character ⁰ is not valid at the start of an identifier" }
int ⁴x = 42;	// { dg-error "extended character ⁴ is not valid at the start of an identifier" }
int ⁵x = 42;	// { dg-error "extended character ⁵ is not valid at the start of an identifier" }
int ⁶x = 42;	// { dg-error "extended character ⁶ is not valid at the start of an identifier" }
int ⁷x = 42;	// { dg-error "extended character ⁷ is not valid at the start of an identifier" }
int ⁸x = 42;	// { dg-error "extended character ⁸ is not valid at the start of an identifier" }
int ⁹x = 42;	// { dg-error "extended character ⁹ is not valid at the start of an identifier" }
int ⁺x = 42;	// { dg-error "extended character ⁺ is not valid at the start of an identifier" }
int ⁻x = 42;	// { dg-error "extended character ⁻ is not valid at the start of an identifier" }
int ⁼x = 42;	// { dg-error "extended character ⁼ is not valid at the start of an identifier" }
int ⁽x = 42;	// { dg-error "extended character ⁽ is not valid at the start of an identifier" }
int ⁾x = 42;	// { dg-error "extended character ⁾ is not valid at the start of an identifier" }
int ₀x = 42;	// { dg-error "extended character ₀ is not valid at the start of an identifier" }
int ₁x = 42;	// { dg-error "extended character ₁ is not valid at the start of an identifier" }
int ₂x = 42;	// { dg-error "extended character ₂ is not valid at the start of an identifier" }
int ₃x = 42;	// { dg-error "extended character ₃ is not valid at the start of an identifier" }
int ₄x = 42;	// { dg-error "extended character ₄ is not valid at the start of an identifier" }
int ₅x = 42;	// { dg-error "extended character ₅ is not valid at the start of an identifier" }
int ₆x = 42;	// { dg-error "extended character ₆ is not valid at the start of an identifier" }
int ₇x = 42;	// { dg-error "extended character ₇ is not valid at the start of an identifier" }
int ₈x = 42;	// { dg-error "extended character ₈ is not valid at the start of an identifier" }
int ₉x = 42;	// { dg-error "extended character ₉ is not valid at the start of an identifier" }
int ₊x = 42;	// { dg-error "extended character ₊ is not valid at the start of an identifier" }
int ₋x = 42;	// { dg-error "extended character ₋ is not valid at the start of an identifier" }
int ₌x = 42;	// { dg-error "extended character ₌ is not valid at the start of an identifier" }
int ₍x = 42;	// { dg-error "extended character ₍ is not valid at the start of an identifier" }
int ₎x = 42;	// { dg-error "extended character ₎ is not valid at the start of an identifier" }
