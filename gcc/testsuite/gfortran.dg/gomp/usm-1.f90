! { dg-do compile }
! { dg-additional-options "-foffload-memory=pinned" }

!$omp requires unified_shared_memory  ! { dg-error "unified_shared_memory at .* is incompatible with the selected -foffload-memory option" }

end
