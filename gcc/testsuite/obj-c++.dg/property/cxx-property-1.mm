/* Testcase from PR obj-c++/48275.  */
/* { dg-do compile } */

@interface Test
{
        int ns;
}
@property (getter=namespace) int ns;

@end
