! { dg-do run }
! PR25419 Default input with commas.
! Derived from example given in PR.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
      stuff = 1
      stuff2 = 2
      write(11,'(a)') ",,"
      rewind(11)
      read(11,*)stuff, stuff2
      if (stuff.ne.1.0) call abort()
      if (stuff2.ne.2.0) call abort()
      rewind (11)
      write(11,'(a)') ","
      rewind(11)
      read(11,*)stuff
      if (stuff.ne.1.0) call abort()
      end

