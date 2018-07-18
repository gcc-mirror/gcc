// Copyright (C) 2002 Free Software Foundation
// Origin: jmr@fulcrummicro.com
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>


struct A;

int main()
{
    A::g();           // { dg-error "incomplete" }
}

