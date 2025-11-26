// { dg-additional-options "-fsyntax-only" }

extern "C" {
    type F;
    type E // { dg-error "failed to parse" }
} // { dg-error "expecting" }
