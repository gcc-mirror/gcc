! { dg-do run }
!
! Test the fix for PR117897 in which the rhs of the pointer assignment at line
! 216 below was marked as being finalizable, contrary to F2023 7.5.6.3 for
! ordinary assignment and certainly wrong in this context.
!
! Contributed by Jean Gual  <jean.gual@cerema.fr>
!
Module  Uef_Classe_Vector
! Ce module implemente le vector de la STL du C++
Private
CHARACTER (len=3),  Parameter :: UEF_PAR_CHAINE_NON_RENSEIGNEE                              = "N_R"
real, parameter          :: UEF_par_vector_progression_ratio = 2
Integer, parameter       :: UEF_par_vector_initial_lenght   = 10

Type, abstract, public :: Uef_Vector_element
    Logical, public         :: m_Element_pointe = .false.
End type Uef_Vector_element

Type, private  :: Uef_Pointeur_element ! Classe pointeur
    Class (Uef_Vector_element), public, pointer  :: m_ptr_element => null()
End type Uef_Pointeur_element

Type, public :: Uef_Vector  ! Vecteur des classes pointeur
    integer  , private                                              :: m_position_fin               = 0
    type(Uef_Pointeur_element), private, allocatable, dimension(:)  :: m_les_pointeur_element
    Character (:), private, allocatable                             :: m_label
    Class (Uef_Vector_element), allocatable, private                :: m_type_element
    logical                                ,private                 :: m_polymorphe = .false.
 Contains
    PROCEDURE :: create                  => Vector_create
    PROCEDURE :: add                     => Vector_add
    PROCEDURE :: Pointer                 => Vector_pointer
    PROCEDURE :: size                    => vector_size
End Type Uef_Vector

Contains
!--------------------
! Vector_create : Cree un vector non deja alloue avec une taille initiale eventuelle
!--------------------
Subroutine Vector_create(le_vector, label, type_element, opt_taille, opt_polymorphe)
!   parametres en entree/sortie
    Class(Uef_Vector),intent (inout)                    :: le_vector
    Character (len=*),intent(in)                        :: label
    Class (Uef_Vector_element),intent(in)               :: type_element
    Integer, intent(in), optional                       :: opt_taille
    Logical, intent(in), optional                       :: opt_polymorphe

!   parametres locaux
    integer                                             :: taille_initiale
!
!-----DEBUT-----------------------------------------------------------------------------------------------------------------------
!    write (*,*) "create:", label
    if (allocated(le_vector%m_les_pointeur_element)) then
        Call Uef_assert(.false., "Vector_create : vecteur deja cree :"// le_vector%m_label)
    endif

    if (present(opt_taille)) then
        taille_initiale = max( 1, opt_taille )
    else
        taille_initiale = UEF_par_vector_initial_lenght
    endif

    if (present(opt_polymorphe)) then
        le_vector%m_polymorphe =  opt_polymorphe
    endif

    allocate( le_vector%m_les_pointeur_element(1:taille_initiale))
    le_vector%m_position_fin                    = 0
    le_vector%m_label                           = label
    allocate (le_vector%m_type_element, source  = type_element)
End Subroutine Vector_create
!--------------------
! Vector_add : ajoute une copie d'un element a la fin du vecteur
!--------------------
Subroutine Vector_add(le_vector, l_element)
!   parametres en entree/sortie
    Class(Uef_Vector),intent(inout)                  :: le_vector
    Class(Uef_Vector_element), intent(in)            :: l_element

!   parametres locaux
    type(Uef_Pointeur_element)                       :: le_ptr_element
!
!-----DEBUT-----------------------------------------------------------------------------------------------------------------------
!
!    write (*,*) "ajout:", le_vector%m_label
    if ( .not. allocated(le_vector%m_les_pointeur_element) ) Then
        Call Vector_create(le_vector, label= UEF_PAR_CHAINE_NON_RENSEIGNEE, type_element = l_element)
    End if
    if ( .not. same_type_as (l_element,le_vector%m_type_element).and. .not. le_vector%m_polymorphe) then
        Call Uef_assert(.false., "Vector_add : element de type incorrect pour :"// le_vector%m_label)
    End if

    if ( le_vector%m_position_fin >= size(le_vector%m_les_pointeur_element) ) then
        call vector_increase_size( le_vector, le_vector%m_position_fin+1 )
    endif

    le_vector%m_position_fin                                = le_vector%m_position_fin + 1
    allocate (le_ptr_element%m_ptr_element, source = l_element)
    le_vector%m_les_pointeur_element(le_vector%m_position_fin) =  le_ptr_element
End Subroutine Vector_add
!--------------------
! vector_size : retourne le nombre d'elements effectifs du vector
!--------------------
Pure Integer Function vector_size(le_vector)
!   parametres en entree
    Class(Uef_Vector), intent (in)       :: le_vector
!
!-----DEBUT-----------------------------------------------------------------------------------------------------------------------
    vector_size = le_vector%m_position_fin
End Function vector_size
!--------------------
! Vector_pointer : pointe sur une valeur
!--------------------
 Function  Vector_pointer( le_vector, position_element )
!   parametres en entree/sortie
    Class(Uef_Vector),intent(inout)         :: le_vector
    integer,intent (in)                     :: position_element
!   parametres en sortie
    Class(Uef_Vector_element), Pointer       :: Vector_pointer
!
!-----DEBUT-----------------------------------------------------------------------------------------------------------------------
!
    if ( position_element < 1 .or. position_element  > le_vector%m_position_fin ) then
        write (*,*) "Vector_pointer : pointage impossible de ", le_vector%m_label, " position_element:",&
                     position_element," size:",le_vector%m_position_fin
        Call Uef_assert(.false., "Vector_pointer : pointage impossible dans "// le_vector%m_label)
    else
        le_vector%m_les_pointeur_element(position_element)%m_ptr_element%m_Element_pointe =.true.
        Vector_pointer => le_vector%m_les_pointeur_element(position_element)%m_ptr_element
    endif
End Function Vector_pointer
!--------------------
! vector_increase_size : augmente la taille du vector
!--------------------
Subroutine vector_increase_size( le_vector, taille_demandee )
!   parametres en entree/sortie
    Class(Uef_Vector),intent(inout)                     :: le_vector
    integer,intent(in)                                  :: taille_demandee
!   Parametres en locaux
    integer                                                 :: Nouvelle_taille, taille_actuelle
    type(Uef_Pointeur_element),dimension (:), allocatable:: tmp_vector
!
!-----DEBUT-----------------------------------------------------------------------------------------------------------------------
!
    taille_actuelle = size(le_vector%m_les_pointeur_element)
    Nouvelle_taille = max(taille_demandee, nint( UEF_par_vector_progression_ratio * taille_actuelle))

    if (Nouvelle_taille > taille_actuelle) then
        allocate(tmp_vector(1:Nouvelle_taille))
        tmp_vector(1:taille_actuelle) = le_vector%m_les_pointeur_element(1:le_vector%m_position_fin)
        call move_alloc(from = tmp_vector  , to = le_vector%m_les_pointeur_element)
    endif
End Subroutine vector_increase_size
!------------------------
Subroutine Uef_Assert (assertion, message)
!--------------------
! traitement des assertions
!--------------------
! Parametres en entree
Logical, Intent(in) ::  assertion
Character (len = *) , intent(in):: message
!-------------------------------------------------------------------------------------------------
    if (.not. assertion ) Then

        write(*,*) message
        write(*,*) " ARRET PREMATURE : PREVENIR LE GESTIONNAIRE"
    stop
    End if
End Subroutine Uef_Assert

End Module  Uef_Classe_Vector

Program Cds_Principal
   Use Uef_Classe_vector
!
!--------------------------------------------------------------------------------------------------
    TYPE, extends(Uef_Vector_element),   abstract :: Cds_Materiau
        Character (len=8)               :: m_Nom_materiau                = "12345678"
        Type(Uef_Vector)                :: m_Les_situations
    END TYPE Cds_Materiau

    Type,   extends (Cds_Materiau)      :: Cds_Materiau_Acier_EC
        Double precision                :: m_Fyk                                = 0.00
    End type Cds_Materiau_Acier_EC

   Type(Uef_Vector)                                    :: Cds_Mod_Les_materiaux
   Type (Cds_Materiau_Acier_EC)                        :: acier_ec
   Class (Cds_Materiau), pointer                       :: pt_materiau
   Character *(8)                                      :: nom_materiau
!-------------------------------------------------------------------------------------------------
      CaLL  Cds_Mod_Les_materiaux%Add (acier_ec)
      nom_materiau = "12345678"
      pt_materiau => Get_pt_materiau_nom (Cds_Mod_Les_materiaux, nom_materiau)
contains

Function Get_Pt_Materiau_nom (vecteur, nom_materiau)
    !--------------------
    !   Fonction :
    !--------------------
     ! Parametres en entree
    Character *(8), Intent (in)        :: nom_materiau
    Type (Uef_Vector)          , Intent (inout)     :: vecteur

    ! Parametres en sortie
    Class  (Cds_Materiau),pointer                    :: Get_Pt_Materiau_nom

    ! Parametres locaux
    Integer                                         :: no_materiau

    Class (Uef_Vector_element),pointer           ::  pt_vector_element
    !--------------------
    do no_materiau = 1 , vecteur%size()
        pt_vector_element => vecteur%Pointer(no_materiau)
! this instruction did not work
         Get_Pt_Materiau_nom => Cds_pt_materiau(pt_vector_element)

        if (trim (Get_Pt_Materiau_nom%m_Nom_materiau) /= '12345678') stop 1
        if (Get_Pt_Materiau_nom%m_Nom_materiau == nom_materiau) Then
            return
        End if
    End do
    Get_Pt_Materiau_nom => null()
End Function Get_Pt_Materiau_nom
!
!--------------------
function Cds_Pt_Materiau(vector_element)
!--------------------
!   Fonction : pointage de la valeur
!--------------------

    ! Parametres en entree
    Class (Uef_Vector_element),intent(in),target   ::  vector_element
    ! Parametres en sortie
    Class(Cds_Materiau), pointer                   ::  Cds_Pt_Materiau
    !-----------------------------------------------------------------------------------------------
    select type(vector_element)
    Class is (Cds_Materiau)
        Cds_Pt_Materiau => vector_element
    class default
        stop 2
    end select
End Function Cds_Pt_Materiau

End Program Cds_Principal
