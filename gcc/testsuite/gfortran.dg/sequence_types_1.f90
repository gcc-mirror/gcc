! { dg-do compile }
! Tests the fix for PR28590, in which pointer components of sequence
! types would give the error that the component is itself not a
! sequence type (4.4.1) if the component was not already defined.
!
! Contributed by Chris Nelson <ccnelson@itacllc.com>
! 
module data_types
  Integer, Parameter :: kindAry    = selected_int_kind(r=8)
  Integer, Parameter :: kindInt    = selected_int_kind(r=8)

  Integer, Parameter :: kindQ      = selected_real_kind(p=6,r=37)
  Integer, Parameter :: kindXYZ    = selected_real_kind(p=13,r=200)
  Integer, Parameter :: kindDouble = selected_real_kind(p=13,r=200)

  type GroupLoadInfo
    sequence
    Integer(kindAry)          :: loadMode
    Integer(kindAry)          :: normalDir
    Real(kindQ)               :: refS, refL, refX, refY, refZ
    Real(kindQ)               :: forcex,   forcey,   forcez 
    Real(kindQ)               :: forcexv,  forceyv,  forcezv 
    Real(kindQ)               :: momx,     momy,     momz 
    Real(kindQ)               :: momxv,    momyv,    momzv 
    Real(kindQ)               :: flmassx,  flmassy,  flmassz 
    Real(kindQ)               :: flmomtmx, flmomtmy, flmomtmz 
    Real(kindQ)               :: flheatN
  end type GroupLoadInfo

  type GroupRigidMotion
    sequence
    Integer(kindInt)                     :: motiontyp
    Real(kindXYZ), dimension(3)          :: xref
    Real(kindXYZ), dimension(3)          :: angCurrent
    Real(kindXYZ), dimension(3)          :: xdot
    Real(kindXYZ), dimension(3)          :: angNew
    Real(kindXYZ), dimension(3)          :: angRate
    Real(kindDouble)                     :: curTim
    Real(kindXYZ)              , pointer :: properties
    Type(PrescribedMotionData) , pointer :: PrescribeDat
  end type GroupRigidMotion

  type PrescribedMotionData
    sequence
    Integer(kindInt)            :: prescr_typ
    Real(kindXYZ), dimension(3) :: xvel
    Real(kindXYZ)               :: amplitude
    Real(kindXYZ)               :: frequency
    Real(kindXYZ)               :: phase
    Real(kindXYZ), dimension(3) :: thetadot
    Real(kindXYZ), dimension(3) :: thetaddot
  end type PrescribedMotionData
  
  type GroupDeformingMotion
    sequence
    Integer(kindAry) :: nmodes
  end type GroupDeformingMotion
  
  type GroupLL
    sequence
    type(GroupLL)             , pointer :: next
    type(GroupLL)             , pointer :: parent
    character(32)                       :: name
    type(GroupDefLL)          , pointer :: entities
    type(GroupLoadInfo)       , pointer :: loadInfo
    type(GroupRigidMotion)    , pointer :: RigidMotion
    type(GroupDeformingMotion), pointer :: DeformingMotion
  end type GroupLL
  
  type GroupDefLL 
    sequence
    type ( GroupDefLL ), pointer            :: next
    Integer(kindInt)                        :: zone
    Integer(kindInt)                        :: surface
    type ( GroupLL ), pointer               :: subGrp
    Integer(kindInt)                        :: normalDir
    Integer(kindInt), dimension(:), pointer :: subset
  end type GroupDefLL
end module data_types
! { dg-final { cleanup-modules "data_types" } }
