with Generic_Inst4_Gen;
with Generic_Inst4_Typ; use Generic_Inst4_Typ;
package Generic_Inst4_Inst is new Generic_Inst4_Gen (
   Param => "SHARING;" & --  ERROR
     Generic_Inst4_Typ.New_Int'image (Generic_Inst4_Typ.T'size/8));
