*lib:
+ %{mrtp:%{!shared: \
     -L%:if-exists-else( \
         %:getenv(WIND_BASE /target/lib/usr/lib/ppc/PPC32/e500v2common) \
         %:getenv(WIND_BASE /target/usr/lib/ppc/PPC32/e500v2common)) \
   }}
