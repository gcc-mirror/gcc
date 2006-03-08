! { dg-do run }
! PR 26554 : Test logical read from string. Test case derived from PR.
! Submitted by Jerry DeLisle <jvdelisle@verizon.net>.
program bug
  implicit none
  character*30 :: strg
  logical l
  l = .true.
  strg = "false"
  read (strg,*) l
  if (l) call abort()
  strg = "true"
  read (strg,*) l
  if (.not.l) call abort()
  end
      