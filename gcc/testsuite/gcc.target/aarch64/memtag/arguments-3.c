/* { dg-do compile } */
/* { dg-additional-options "--param hwasan-random-frame-tag=0" } */
/* { dg-warning "is ignored when \'-fsanitize=memtag-stack\' is present" "" { target *-*-* } 0 } */
