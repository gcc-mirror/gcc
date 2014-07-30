*self_spec:
+ %{!auto-register:%{!noauto-register:-auto-register}} \
  %{!crtbe:%{!nocrtbe:-crtbe}}

*startfile:
+ %{crtbe:%{!nocrtbe: \
    %{!noauto-register:crtbegin.o%s} \
    %{noauto-register:crtbeginT.o%s} \
   }}

*endfile:
+ %{crtbe:%{!nocrtbe:crtend.o%s}}

