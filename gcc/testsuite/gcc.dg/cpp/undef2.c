/* C99 6.10.8 para 4: None of [the predefined macro names] shall be
   the subject of a #define or an #undef preprocessing directive.  */

/* { dg-do preprocess } */

#undef __DATE__		/* { dg-warning "undefining \"__DATE__\"" } */
#undef __TIME__		/* { dg-warning "undefining \"__TIME__\"" } */
#undef __FILE__		/* { dg-warning "undefining \"__FILE__\"" } */
#undef __LINE__		/* { dg-warning "undefining \"__LINE__\"" } */
#undef __STDC__		/* { dg-warning "undefining \"__STDC__\"" } */

/* These should be protected from #undef, but aren't, because they
   are set with normal #define commands - and on top of that, some
   of them are library properties, outside our control.  To consider:
   warn about undefining/redefining any identifier beginning with
   __STDC_ .

   __STDC_HOSTED__
   __STDC_VERSION__
   __STDC_IEC_559__
   __STDC_IEC_559_COMPLEX__
   __STDC_ISO_10646__
 */
