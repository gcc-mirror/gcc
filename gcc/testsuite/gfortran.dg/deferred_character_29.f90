! { dg-do compile }
!
! Test the fix for PR83196 comment #4 (there by mistake)
!
! Contributed by Arjen Markus  <arjen.markus895@gmail.com>
!____________________________________________________________
! keyindex.f90 --
!     Class implementing a straightforward keyword/index list
!     The idea is to have a very simple implementation to
!     store keywords (strings) and return the position in the
!     list or vice versa.
!____________________________________________________________
module keyindices
    implicit none

    private

    integer, parameter                              :: default_keylength = 40

    type keyindex
        integer                                     :: keylength
        integer                                     :: lastindex = 0
        character(len=:), dimension(:), allocatable :: keyword
    contains
        procedure                                   :: init      => init_keyindex
        procedure                                   :: get_index => get_index_from_list
        procedure                                   :: get_key   => get_keyword_from_list
        procedure                                   :: has_key   => has_keyword_in_list
    end type keyindex

    public :: keyindex
contains

! init_keyindex --
!     Initialise the object
!
! Arguments:
!     this                     Keyindex object
!     initial_size             Initial size of the list (optimisation)
!     keylength                Maximum length of a keyword (optional)
!
subroutine init_keyindex( this, initial_size, keylength )
    class(keyindex), intent(inout) :: this
    integer, intent(in)           :: initial_size
    integer, intent(in), optional :: keylength

    integer                       :: keylength_

    if ( present(keylength) ) then
        keylength_ = keylength
    else
        keylength_ = default_keylength
    endif

    !
    ! Allocate the list of keywords
    !
    if ( allocated(this%keyword) ) then
        deallocate( this%keyword )
    endif


    allocate( character(len=keylength_):: this%keyword(initial_size) )

    this%lastindex = 0
    this%keylength = keylength_
end subroutine init_keyindex

! get_index_from_list --
!     Look up the keyword in the list and return its index
!
! Arguments:
!     this                     Keyindex object
!     keyword                  Keyword to be looked up
!
! Returns:
!     Index in the list
!
! Note:
!     If the keyword does not yet exist, add it to the list
!
integer function get_index_from_list( this, keyword )
    class(keyindex), intent(inout) :: this
    character(len=*), intent(in)  :: keyword

    integer                       :: i
    character(len=this%keylength), dimension(:), allocatable :: newlist

    if ( .not. allocated(this%keyword) ) then
        call this%init( 50 )
    endif

    get_index_from_list = 0

    do i = 1,this%lastindex
        if ( this%keyword(i) == keyword ) then
            get_index_from_list = i
            exit
        endif
    enddo

    !
    ! Do we need to add it?
    !
    if ( get_index_from_list == 0 ) then
        if ( size(this%keyword) <= this%lastindex ) then
            !
            ! Allocate a larger list
            !
            allocate( character(len=this%keylength):: newlist(2*size(this%keyword)) )

            newlist(1:size(this%keyword)) = this%keyword
            call move_alloc( newlist, this%keyword )
        endif

        get_index_from_list = this%lastindex + 1
        this%lastindex      = get_index_from_list
        this%keyword(get_index_from_list) = keyword
    endif
end function get_index_from_list

! get_keyword_from_list --
!     Look up the keyword in the list by the given index
!
! Arguments:
!     this                     Keyindex object
!     idx                      Index of the keyword
!
! Returns:
!     Keyword as stored in the list
!
! Note:
!     If the index does not exist, an empty string is returned
!
function get_keyword_from_list( this, idx )
    class(keyindex), intent(inout) :: this
    integer, intent(in)            :: idx

    character(len=this%keylength)  :: get_keyword_from_list

    get_keyword_from_list = ' '

    if ( idx >= 1 .and. idx <= this%lastindex ) then
        get_keyword_from_list = this%keyword(idx)
    endif
end function get_keyword_from_list

! has_keyword_in_list --
!     Look up whether the keyword is stored in the list or not
!
! Arguments:
!     this                     Keyindex object
!     keyword                  Keyword to be looked up
!
! Returns:
!     True if the keyword is in the list or false if not
!
logical function has_keyword_in_list( this, keyword )
    class(keyindex), intent(inout) :: this
    character(len=*), intent(in)  :: keyword

    integer                       :: i

    has_keyword_in_list = .false.

    do i = 1,this%lastindex
        if ( this%keyword(i) == keyword ) then
            has_keyword_in_list = .true.
            exit
        endif
    enddo
end function has_keyword_in_list

end module keyindices

    use keyindices
    type(keyindex) :: idx

    call idx%init (3, 8)

    if (idx%get_index ("one") .ne. 1) stop 1
    if (idx%get_index ("two") .ne. 2) stop 2
    if (idx%get_index ("three") .ne. 3) stop 3

! Check that new span is generated as list is extended.
    if (idx%get_index ("four") .ne. 4) stop 4
    if (idx%get_index ("five") .ne. 5) stop 5
    if (idx%get_index ("six") .ne. 6) stop 6

! Search by keyword
    if (.not.idx%has_key ("four")) stop 7
    if (idx%has_key ("seven")) stop 8

! Search by index
    if (idx%get_key (4) .ne. "four") stop 9
    if (idx%get_key (10) .ne. "") stop 10
end