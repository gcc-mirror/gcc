! PR 13372 -- we incorrectly added a symbol for p, which broke implicit typing
module t
implicit none
integer, parameter :: F = selected_real_kind(P =  6, R =  37)
end module t

