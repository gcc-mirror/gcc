package Opt8 is

    type Value_Number_Kind is
      (Int_Literal_VN,
       Selected_Address_VN,
       Membership_VN,
       Initial_External_Kappa_VN,
       Aliased_Kappa_VN,
       Phi_As_Kappa_VN,
       Multi_Target_Call_Kappa_VN,
       Final_Value_Of_Seq_Kappa_VN,
       Block_Kappa_VN);

    subtype Kappa_VN is Value_Number_Kind
    range Initial_External_Kappa_VN .. Block_Kappa_VN;

    type Value_Number_Id is new Positive;

    type Kappa_Component_Rec;

    type Kappa_Component_Ptr is access Kappa_Component_Rec;

    type Kappa_Component_Rec is record
        Content_VN : Value_Number_Id;
        Next : Kappa_Component_Ptr;
    end record;

    type Value_Number_Rec(Kind : Value_Number_Kind) is record
        Id: Value_Number_Id;
        case Kind is
            when Int_Literal_VN =>
                Int_Val : Integer;
            when Kappa_VN =>
                Old_Value : Kappa_Component_Rec;
                Possible_New_Values : Kappa_Component_Ptr;
                Use_Default : Boolean;
            when Others =>
                null;
        end case;
    end record;

    type Value_Number is access all Value_Number_Rec;

    function VN_Complexity (Val : Value_Number; N : Natural) return Natural;

end Opt8;
