! { dg-do run }     
! Test of fix to bug triggered by NIST fm908.for.
! Left tabbing, followed by X or T-tabbing to the right would
! cause spaces to be overwritten on output data.
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
  program tl_editting
    character*10           ::  line
    character*10           ::  aline = "abcdefxyij"
    character*2            ::  bline = "gh"
    character*10           ::  cline = "abcdefghij"
    write (line, '(a10,tl6,2x,a2)') aline, bline
    if (line.ne.cline) call abort ()
  end program tl_editting

