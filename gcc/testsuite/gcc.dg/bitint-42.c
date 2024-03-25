/* PR middle-end/112679 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-frounding-math" } */

float
foo (void)
{
  return 0x353eab28b46b03ea99b84f9736cd8dbe5e986915a0383c3cb381c0da41e31b3621c75fd53262bfcb1b0e6251dbf00f3988784e29b08b65640c263e4d0959832a20e2ff5245be1e60uwb;
}
