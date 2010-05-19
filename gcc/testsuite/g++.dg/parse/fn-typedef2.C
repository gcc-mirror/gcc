// Test that merge_types preserves fn cv-quals.

typedef void ft() const;
typedef void V;
typedef V ft() const;

ft f;				// { dg-error "qualified" }
