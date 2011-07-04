#define PROGMEM __attribute__((progmem))

#define PSTR(s)                                             \
    (__extension__({                                        \
            static const char __c[] PROGMEM = (s);          \
            &__c[0];}))

#define pgm_read_char(addr)                                 \
    (__extension__({                                        \
            unsigned int __addr16 = (unsigned int)(addr);   \
            char __result;                                  \
            __asm__ ("lpm %0, %a1"                          \
                     : "=r" (__result) : "z" (__addr16));   \
            __result; }))
