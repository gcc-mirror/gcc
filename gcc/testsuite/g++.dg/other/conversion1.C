// PR C++/2213
// Origin: philippeb@corel.com
// Copyright (C), 2002 Free Software Foundation
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

// { dg-do compile } 
 
class QObject
{
};

int main()
{
  long long m;
  
  (void (QObject::*)()) m;    // { dg-error "invalid cast" "" }
}
