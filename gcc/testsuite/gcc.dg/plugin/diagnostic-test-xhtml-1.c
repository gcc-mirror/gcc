/* { dg-do compile } */

int missing_semicolon (void)
{
  return 42
}

/* Verify some properties of the generated HTML.  */

/* { dg-begin-multiline-output "" }
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html
     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
   { dg-end-multiline-output "" } */

/* { dg-excess-errors "" } */
