void f (int i)
{
 __asm__("" : );
 __asm__("" : "+g" (i));

 __asm__("" :: );
 __asm__("" :: "g" (i));

 __asm__("" : : );
 __asm__("" : "+g" (i) : );
 __asm__("" : : "g" (i));
 __asm__("" : "+g" (i) :  "g" (i));

 __asm__("" ::: );
 __asm__("" ::: "memory");

 __asm__("" : :: );
 __asm__("" : "+g" (i) :: );
 __asm__("" : :: "memory");
 __asm__("" : "+g" (i) :: "memory");

 __asm__("" :: : );
 __asm__("" :: "g" (i) : );
 __asm__("" :: : "memory");
 __asm__("" :: "g" (i) : "memory");

 __asm__("" : : : );
 __asm__("" : "+g" (i) : : );
 __asm__("" : : "g" (i) : );
 __asm__("" : : : "memory");
 __asm__("" : "+g" (i) : "g" (i) : );
 __asm__("" : "+g" (i) : : "memory");
 __asm__("" : : "g" (i) : "memory");
 __asm__("" : "+g" (i) : "g" (i) : "memory");
}
