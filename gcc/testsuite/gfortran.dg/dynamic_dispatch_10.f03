! { dg-do run }
!
! [OOP] Fortran runtime error: internal error: bad hash value in dynamic dispatch
!
! Contributed by David Car <david.car7@gmail.com>

module BaseStrategy

  type, public, abstract :: Strategy
   contains
     procedure(strategy_update), pass( this ), deferred :: update
     procedure(strategy_pre_update), pass( this ), deferred :: preUpdate
     procedure(strategy_post_update), pass( this ), deferred :: postUpdate
  end type Strategy

  abstract interface
     subroutine strategy_update( this )
       import Strategy
       class (Strategy), target, intent(in) :: this
     end subroutine strategy_update
  end interface

  abstract interface
     subroutine strategy_pre_update( this )
       import Strategy
       class (Strategy), target, intent(in) :: this
     end subroutine strategy_pre_update
  end interface

  abstract interface
     subroutine strategy_post_update( this )
       import Strategy
       class (Strategy), target, intent(in) :: this
     end subroutine strategy_post_update
  end interface
     
end module BaseStrategy

!==============================================================================

module LaxWendroffStrategy

  use BaseStrategy

  private :: update, preUpdate, postUpdate

  type, public, extends( Strategy ) :: LaxWendroff
     class (Strategy), pointer :: child => null()
     contains
       procedure, pass( this ) :: update
       procedure, pass( this ) :: preUpdate
       procedure, pass( this ) :: postUpdate
  end type LaxWendroff

contains

  subroutine update( this )
    class (LaxWendroff), target, intent(in) :: this

    print *, 'Calling LaxWendroff update'
  end subroutine update

  subroutine preUpdate( this )
    class (LaxWendroff), target, intent(in) :: this
    
    print *, 'Calling LaxWendroff preUpdate'
  end subroutine preUpdate

  subroutine postUpdate( this )
    class (LaxWendroff), target, intent(in) :: this
    
    print *, 'Calling LaxWendroff postUpdate'
  end subroutine postUpdate
  
end module LaxWendroffStrategy

!==============================================================================

module KEStrategy

  use BaseStrategy
  ! Uncomment the line below and it runs fine
  ! use LaxWendroffStrategy

  private :: update, preUpdate, postUpdate

  type, public, extends( Strategy ) :: KE
     class (Strategy), pointer :: child => null()
     contains
       procedure, pass( this ) :: update
       procedure, pass( this ) :: preUpdate
       procedure, pass( this ) :: postUpdate
  end type KE
  
contains

  subroutine init( this, other )
    class (KE), intent(inout) :: this
    class (Strategy), target, intent(in) :: other

    this % child => other
  end subroutine init

  subroutine update( this )
    class (KE), target, intent(in) :: this

    if ( associated( this % child ) ) then
       call this % child % update()
    end if

    print *, 'Calling KE update'
  end subroutine update

 subroutine preUpdate( this )
    class (KE), target, intent(in) :: this
    
    if ( associated( this % child ) ) then
       call this % child % preUpdate()
    end if

    print *, 'Calling KE preUpdate'
  end subroutine preUpdate

  subroutine postUpdate( this )
    class (KE), target, intent(in) :: this

    if ( associated( this % child ) ) then
       call this % child % postUpdate()
    end if
    
    print *, 'Calling KE postUpdate'
  end subroutine postUpdate
  
end module KEStrategy

!==============================================================================

program main

  use LaxWendroffStrategy
  use KEStrategy

  type :: StratSeq
     class (Strategy), pointer :: strat => null()
  end type StratSeq

  type (LaxWendroff), target :: lw_strat
  type (KE), target :: ke_strat

  type (StratSeq), allocatable, dimension( : ) :: seq
  
  allocate( seq(10) )

  call init( ke_strat, lw_strat )
  call ke_strat % preUpdate()
  call ke_strat % update()
  call ke_strat % postUpdate()
  ! call lw_strat % update()

  seq( 1 ) % strat => ke_strat
  seq( 2 ) % strat => lw_strat

  call seq( 1 ) % strat % update()

  do i = 1, 2
     call seq( i ) % strat % update()
  end do

end

! { dg-final { cleanup-modules "BaseStrategy LaxWendroffStrategy KEStrategy" } }
