/* { dg-do preprocess } */
/* { dg-additional-options "-M -MQ b\\\$ob -MT b\\\$ill" } */

int i;

/* { dg-final { scan-file pr90927.i {b\$ill b\$\$ob:} } } */
