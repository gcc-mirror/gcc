/* { dg-options "-fdiagnostics-show-caret" } */

/* Various versions of the same C error, with a variety of line spacing,
   and of columns, to exercise the line-span handling in
   diagnostic-show-locus.c (PR other/69554).  */

/* All on one line.  */

int test_1 (const char *p, const char *q)
{
  return (p + 1) + (q + 1); /* { dg-error "invalid operands" } */
/* { dg-begin-multiline-output "" }
   return (p + 1) + (q + 1);
          ~~~~~~~ ^ ~~~~~~~
             |         |
             |         const char *
             const char *
   { dg-end-multiline-output "" } */
}

/* On separate lines, but without intervening lines.
   This can be printed as a single span of lines.  */

int test_2 (const char *p, const char *q)
{
  return (p + 1)
           +  /* { dg-error "invalid operands" } */
            (q + 1);
/* { dg-begin-multiline-output "" }
   return (p + 1)
          ~~~~~~~
             |
             const char *
            +
            ^
             (q + 1);
             ~~~~~~~
                |
                const char *
   { dg-end-multiline-output "" } */
}

/* On separate lines, with an intervening line between lines 1 and 2.
   This is printed as 2 "spans" of lines, broken up by the intervening
   line.  */

int test_3 (const char *p, const char *q)
{
  return (p + 1) /* { dg-locus "10" } */

           +  /* { dg-error "invalid operands" } */
             (q + 1);
/* { dg-locus "12" "" { target *-*-* } "51" } */
/* { dg-begin-multiline-output "" }
   return (p + 1)
          ~~~~~~~
             |
             const char *
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
            +
            ^
              (q + 1);
              ~~~~~~~
                 |
                 const char *
   { dg-end-multiline-output "" } */
}

/* As above, but the intervening line is between lines 2 and 3,
   so that the 2 spans are grouped the other way.  */

int test_4 (const char *p, const char *q)
{
  return (p + 1)
           +  /* { dg-error "invalid operands" } */

             (q + 1); /* { dg-locus "14" } */
/* { dg-begin-multiline-output "" }
   return (p + 1)
          ~~~~~~~
             |
             const char *
            +
            ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
              (q + 1);
              ~~~~~~~
                 |
                 const char *
   { dg-end-multiline-output "" } */
}

/* On separate lines, with intervening lines.
   This is printed as 3 "spans" of lines, each span being an
   individual line.  */

int test_5 (const char *p, const char *q)
{
  return (p + 1) /* { dg-locus "10" } */

           +  /* { dg-error "invalid operands" } */

             (q + 1); /* { dg-locus "14" } */
/* { dg-locus "12" "" { target *-*-* } "103" } */
/* { dg-begin-multiline-output "" }
   return (p + 1)
          ~~~~~~~
             |
             const char *
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
            +
            ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
              (q + 1);
              ~~~~~~~
                 |
                 const char *
   { dg-end-multiline-output "" } */
}

/* On separate lines, with numerous intervening lines.
   This is printed as 3 "spans" of lines, each span being an
   individual line.  */

int test_6 (const char *p, const char *q)
{
  return (p + 1) /* { dg-locus "10" } */
	  /* Lorem ipsum dolor sit amet, consectetur adipiscing elit.
	     Maecenas nisl sapien, rutrum non euismod et, rutrum ac felis.
	     Morbi nec nisi ipsum. Quisque pulvinar ante nec urna rhoncus,
	     a cursus nisi commodo. Praesent euismod neque lectus, at
	     dapibus ipsum gravida in. Pellentesque tempor massa eu viverra
	     feugiat. Proin eleifend pulvinar urna, ut dapibus metus vehicula
	     ac. Suspendisse rutrum finibus quam, ac dignissim diam blandit
	     maximus. In blandit viverra pulvinar. Praesent vel tellus
	     elementum, placerat lacus quis, ornare lectus. Donec ac
	     eleifend nulla, sit amet condimentum risus. Vestibulum aliquam
	     maximus ante non pellentesque. Praesent mollis ante in risus
	     feugiat hendrerit. Praesent feugiat maximus urna nec blandit. */
           +  /* { dg-error "invalid operands" } */
	  /* Vestibulum ac nunc eget enim tempor tristique. Suspendisse
	     potenti. Nam et sollicitudin enim. Morbi sed tincidunt lectus.
	     Sed facilisis velit at ante maximus feugiat. Sed vestibulum mi
	     id leo tempor, sed ullamcorper sapien efficitur. Vestibulum purus
	     lacus, dignissim non magna at, tincidunt luctus nisl. Cum sociis
	     natoque penatibus et magnis dis parturient montes, nascetur
	     ridiculus mus. Donec elit elit, laoreet a dolor quis, eleifend
	     dapibus metus. Proin lectus turpis, eleifend nec pharetra eu,
	     fermentum in lacus. Morbi sit amet mauris orci. Nam sagittis,
	     nibh vel fermentum dictum, purus ex hendrerit odio, feugiat
	     fringilla sapien elit vitae nisl. Fusce mattis commodo risus
	     nec convallis. */
             (q + 1); /* { dg-locus "14" } */
/* { dg-locus "12" "" { target *-*-* } "144" } */
/* { dg-begin-multiline-output "" }
   return (p + 1)
          ~~~~~~~
             |
             const char *
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
            +
            ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
              (q + 1);
              ~~~~~~~
                 |
                 const char *
   { dg-end-multiline-output "" } */
}
