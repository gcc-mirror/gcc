*lib:
+ %{mrtp:%{!shared: \
      -L%:getenv(WIND_BASE /target/lib/usr/lib/arm/ARMARCH7/common) \
   }}
