! { dg-do run }
! PR93234 Inquire by UNIT on preopened unit failed on ROUND= and SIGN=
program inquire_browse
implicit none
integer                              :: ios
character(len=256)                   :: message
   !==============================================================================================
      character(len=20)              :: name           ; namelist/inquire/name
      integer                        :: unit           ; namelist/inquire/unit
      integer                        :: id             ; namelist/inquire/id
   !==============================================================================================
      integer                        :: recl           ; namelist/inquire/recl
      integer                        :: nextrec        ; namelist/inquire/nextrec
      integer                        :: pos            ; namelist/inquire/pos
      integer                        :: size           ; namelist/inquire/size
   !==============================================================================================
   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
      character(len=20)              :: access         ; namelist/inquire/access
      character(len=20)              :: sequential     ; namelist/inquire/sequential
      character(len=20)              :: stream         ; namelist/inquire/stream
      character(len=20)              :: direct         ; namelist/inquire/direct
   !  ACTION    =  READ        | WRITE         |  READWRITE
      character(len=20)              :: action         ; namelist/inquire/action
      character(len=20)              :: read           ; namelist/inquire/read
      character(len=20)              :: write          ; namelist/inquire/write
      character(len=20)              :: readwrite      ; namelist/inquire/readwrite
   !  FORM      =  FORMATTED   |  UNFORMATTED
      cHaracter(len=20)              :: form           ; namelist/inquire/form
      character(len=20)              :: formatted      ; namelist/inquire/formatted
      character(len=20)              :: unformatted    ; namelist/inquire/unformatted
   !  POSITION  =  ASIS        |  REWIND       |  APPEND
      character(len=20)              :: position       ; namelist/inquire/position
   !==============================================================================================
      character(len=20)              :: blank          ; namelist/inquire/blank
      character(len=20)              :: decimal        ; namelist/inquire/decimal
      character(len=20)              :: sign           ; namelist/inquire/sign
      character(len=20)              :: round          ; namelist/inquire/round
      character(len=20)              :: delim          ; namelist/inquire/delim
      character(len=20)              :: encoding       ; namelist/inquire/encoding
      character(len=20)              :: pad            ; namelist/inquire/pad
   !==============================================================================================
      logical                        :: named          ; namelist/inquire/named
      logical                        :: opened         ; namelist/inquire/opened
      logical                        :: exist          ; namelist/inquire/exist
      integer                        :: number         ; namelist/inquire/number
      logical                        :: pending        ; namelist/inquire/pending
      character(len=20)              :: asynchronous   ; namelist/inquire/asynchronous
   !==============================================================================================
   unit=5
   !!include "setunit_and_open.inc"
   inquire(unit=unit,sign=sign)
   inquire(unit=unit,round=round)
         inquire(unit=unit,                                                                              &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   name=name,position=position,                                                                    &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
999  continue
     if(ios.eq.0)then
        !write(*,nml=inquire,delim='none')
     else
        stop 1
     endif
end program inquire_browse
