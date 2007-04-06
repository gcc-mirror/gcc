package equal1 is
  type Basic_Connection_Status_T is (Connected, Temporary_Disconnected,
     Disconnected);
  for Basic_Connection_Status_T'Size use 8;
  type Application_Connection_Status_T is (Connected, Disconnected);
  for Application_Connection_Status_T'Size use 8;
end equal1;

