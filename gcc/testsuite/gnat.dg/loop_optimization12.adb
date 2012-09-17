-- { dg-do compile }
-- { dg-options "-O2" }

package body Loop_Optimization12 is

  procedure Reset (S : Rec_Ptr) is
  begin
    for I in Enum1 loop
      S.F (I).all := (others =>
                       (others =>
                         (others =>
                           (others =>
                             (others =>
                               (others =>
                                 (others =>
                                   (others =>
                                    (others =>
                                      (others => 0))))))))));
    end loop;
  end;

end Loop_Optimization12;
