/* Test for segfault handling an empty attribute.  */
/* Origin: PR c/4294 from <tori@ringstrom.mine.nu>.  */

void __attribute__(()) foo();
