/* { dg-do compile } */

@interface A
{
  ]  /* { dg-error "xpected" } */
}
@end

@interface B
{
  ];  /* { dg-error "xpected" } */
}
@end

@interface C
{
  ];  /* { dg-error "xpected" } */
  int x;
}
@end

@interface D
{
  )  /* { dg-error "xpected" } */
}
@end

@interface E
{
  );  /* { dg-error "xpected" } */
}
@end

@interface F
{
  );  /* { dg-error "xpected" } */
  int x;
}
@end
