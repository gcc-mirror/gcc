// { dg-options "-frust-name-resolution-2.0" }

struct Test; // { dg-warning "struct is never constructed: .Test." }

impl Test {}
