--  { dg-do compile }

package limited1 is
   pragma Pure;
   
   type Buffer is limited interface;
   type Synchronous_Buffer_Type is synchronized interface and Buffer;

private
end limited1;
