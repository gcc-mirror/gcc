// { dg-do run  }
// { dg-options "-ansi" }
// prms-id: 5958

class A { };

main() {
  int i = 1;
  if (1 not_eq 1)
    return 1;
  if (not (1 and 1))
    return 1;
  if (not (1 or 1))
    return 1;
  if (compl ~0)
    return 1;
  if (1 bitand 2)
    return 1;
  if (not (1 bitor 2))
    return 1;
  if (1 xor 1)
    return 1;
  i and_eq 1;
  i or_eq 2;
  i xor_eq 4;
  if (i not_eq 7)
    return 1;
}
