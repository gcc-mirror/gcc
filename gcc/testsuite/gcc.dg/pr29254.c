/* { dg-do compile } */
/* { dg-options "-O3 -Werror" } */
/* { dg-warning "warnings being treated as errors" "" {target "*-*-*"} 0 } */

list_compare (int * list1)
{
  if (list1)
    value_compare ();
}

func1 (int * f){}

value_compare (int * a)
{
    if (a)
        list_compare (a);
}

func2 (const int * fb)
{
  func1 ((int *) fb); /* { dg-warning "discards qualifiers" } */
}
