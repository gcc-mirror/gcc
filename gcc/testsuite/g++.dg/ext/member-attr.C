/* Test to see if__attribute__'s are handled by inline member functions */
/* { dg-do compile } */
/* { dg-options "-fmessage-length=0" } */

/* Previously __attribute__'s were handled by the grammar but "dropped
   on the floor", these effectively ignoring them.  This tests the fix
   to see that they are now handled.  In this test it should report
   that we have an illegal attribute.  */

class T {
  public:
    __attribute__ ((garbage1)) void member1(int) {} /* { dg-error "`garbage1' attribute directive ignored" "" } */
    void __attribute__ ((garbage2)) member2(int) {} /* { dg-error "`garbage2' attribute directive ignored" "" } */
};
