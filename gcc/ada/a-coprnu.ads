------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       ADA.CONTAINERS.PRIME_NUMBERS                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Containers.Prime_Numbers is
pragma Pure (Prime_Numbers);

   type Primes_Type is array (Positive range <>) of Hash_Type;

   Primes : constant Primes_Type :=
     (53,         97,         193,       389,       769,
      1543,       3079,       6151,      12289,     24593,
      49157,      98317,      196613,    393241,    786433,
      1572869,    3145739,    6291469,   12582917,  25165843,
      50331653,   100663319,  201326611, 402653189, 805306457,
      1610612741, 3221225473, 4294967291);

   function To_Prime (Length : Count_Type) return Hash_Type;

end Ada.Containers.Prime_Numbers;
