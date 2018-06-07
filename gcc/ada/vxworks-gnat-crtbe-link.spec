*self_spec:
+ %{!auto-register:%{!noauto-register:-auto-register}} \
  %{!crtbe:%{!nocrtbe:-crtbe}}

*startfile:
+ %{crtbe:%{!nocrtbe: \
    %{mrtp:-l:vx_crtbegin_attr.o%s} \
    %{!mrtp: \
      %{auto-register:-l:vx_crtbegin_array.o%s} \
      %{!auto-register:-l:vx_crtbegin.o%s} \
     } \
   }}

*endfile:
+ %{crtbe:%{!nocrtbe:-l:vx_crtend.o%s}}

