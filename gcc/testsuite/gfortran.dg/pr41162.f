! { dg-do compile }
! PRs 41154/41162
      write (*,'(1PD24.15,F4.2,0P)') 1.0d0
      write (*,'(1PD24.15,F4.2,0P/)') 1.0d0
      end
