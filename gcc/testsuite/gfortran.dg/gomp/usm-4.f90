! { dg-do compile }
! { dg-additional-options "-foffload-memory=pinned" }

!$omp requires unified_address  ! { dg-error "unified_address at .* is incompatible with the selected -foffload-memory option" }

end
