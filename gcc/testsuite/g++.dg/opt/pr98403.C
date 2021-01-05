// { dg-options "-Og -fcse-follow-jumps -fipa-ra" }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-march=goldmont -fPIC -mforce-indirect-call" { target { { i?86-*-* x86_64-*-* } && fpic } } }

enum WindowClass { WC_NONE, WC_AI_SETTINGS, WC_AI_LIST };
enum { AWV_DECREASE, AWV_INCREASE };
enum WidgetType {
  WWT_PANEL,
  WWT_TEXT,
  WWT_MATRIX,
  WWT_FRAME,
  WWT_CAPTION,
  WWT_DEFSIZEBOX,
  WWT_RESIZEBOX,
  WWT_CLOSEBOX,
  NWID_HORIZONTAL,
  NWID_VERTICAL,
  NWID_SPACER,
  NWID_VSCROLLBAR,
  WWT_PUSHTXTBTN,
  WWT_PUSHARROWBTN
};
enum NWidContainerFlags { NC_NONE };
struct NWidgetPartPIP {
  char prepost;
};
struct NWidgetPart {
  NWidgetPartPIP pip;
} __trans_tmp_1;
static NWidgetPart SetResize(short, short) {
  NWidgetPart part;
  return part;
}
NWidgetPart SetMinimalSize(short, short);
static NWidgetPart SetFill(int, int) {
  NWidgetPart part;
  return part;
}
static NWidgetPart EndContainer() {
  NWidgetPart part;
  return part;
}
static NWidgetPart SetDataTip(int, int) {
  NWidgetPart part;
  return part;
}
static NWidgetPart SetMatrixDataTip(char, char, int) { return __trans_tmp_1; }
NWidgetPart SetPadding();
NWidgetPart SetScrollbar(int);
NWidgetPart NWidget(WidgetType, NWidContainerFlags = NC_NONE);
struct WindowDesc {
  WindowDesc(const char *, short, short, WindowClass, WindowClass, int,
             const NWidgetPart *, short, int * = nullptr);
  ~WindowDesc();
};
class CommandCost {
public:
  CommandCost(int);
} const CMD_ERROR(5);
enum { WID_AIC_SCROLLBAR };
const NWidgetPart _nested_ai_list_widgets[]{NWidget(NWID_HORIZONTAL),
                                            NWidget(WWT_CLOSEBOX),
                                            NWidget(WWT_CAPTION),
                                            SetDataTip(8, 4),
                                            NWidget(WWT_DEFSIZEBOX),
                                            NWidget(NWID_HORIZONTAL),
                                            NWidget(WWT_MATRIX),
                                            SetMinimalSize(8, 2),
                                            SetFill(1, 1),
                                            SetResize(1, 1),
                                            SetMatrixDataTip(1, 0, 1),
                                            EndContainer(),
                                            NWidget(WWT_PANEL),
                                            EndContainer(),
                                            NWidget(NWID_HORIZONTAL),
                                            NWidget(NWID_HORIZONTAL),
                                            NWidget(WWT_PUSHTXTBTN),
                                            SetResize(1, 0),
                                            SetFill(1, 0),
                                            SetDataTip(5, 0),
                                            NWidget(WWT_PUSHTXTBTN),
                                            SetResize(1, 0),
                                            SetFill(1, 0),
                                            SetDataTip(1, 2),
                                            EndContainer(),
                                            NWidget(WWT_RESIZEBOX),
                                            EndContainer()};
static WindowDesc _ai_list_desc("", 0, 4, WC_AI_LIST, WC_NONE, 0,
                                _nested_ai_list_widgets, 0);
const NWidgetPart _nested_ai_settings_widgets[]{NWidget(NWID_HORIZONTAL),
                                                NWidget(WWT_CLOSEBOX),
                                                NWidget(WWT_CAPTION),
                                                SetDataTip(0, 4),
                                                NWidget(WWT_DEFSIZEBOX),
                                                EndContainer(),
                                                NWidget(NWID_HORIZONTAL),
                                                NWidget(WWT_MATRIX),
                                                SetMinimalSize(8, 2),
                                                SetResize(1, 1),
                                                SetFill(1, 0),
                                                SetMatrixDataTip(1, 0, 0),
                                                EndContainer(),
                                                NWidget(NWID_HORIZONTAL),
                                                NWidget(NWID_HORIZONTAL),
                                                NWidget(WWT_PUSHTXTBTN),
                                                SetResize(1, 0),
                                                SetDataTip(3, 0),
                                                NWidget(WWT_PUSHTXTBTN),
                                                SetResize(1, 0),
                                                SetDataTip(4, 0),
                                                EndContainer(),
                                                NWidget(WWT_RESIZEBOX),
                                                EndContainer()};
static WindowDesc _ai_settings_desc("", 0, 208, WC_AI_SETTINGS, WC_NONE, 0,
                                    _nested_ai_settings_widgets, 0);
NWidgetPart _nested_ai_config_widgets[]{NWidget(NWID_HORIZONTAL),
                                        NWidget(WWT_CLOSEBOX),
                                        NWidget(WWT_CAPTION),
                                        SetDataTip(5, 4),
                                        EndContainer(),
                                        NWidget(WWT_PANEL),
                                        NWidget(NWID_VERTICAL),
                                        SetPadding(),
                                        NWidget(NWID_HORIZONTAL),
                                        SetPadding(),
                                        NWidget(WWT_PUSHARROWBTN),
                                        SetFill(0, 1),
                                        SetDataTip(AWV_DECREASE, 0),
                                        NWidget(WWT_PUSHARROWBTN),
                                        SetFill(0, 1),
                                        SetDataTip(AWV_INCREASE, 0),
                                        NWidget(NWID_SPACER),
                                        SetMinimalSize(6, 0),
                                        NWidget(WWT_TEXT),
                                        SetDataTip(3, 0),
                                        SetFill(1, 0),
                                        SetPadding(),
                                        EndContainer(),
                                        NWidget(NWID_HORIZONTAL),
                                        SetPadding(),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetResize(1, 0),
                                        SetFill(1, 0),
                                        SetDataTip(1, 2),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetResize(1, 0),
                                        SetFill(1, 0),
                                        SetDataTip(3, 4),
                                        EndContainer(),
                                        EndContainer(),
                                        SetPadding(),
                                        NWidget(NWID_HORIZONTAL),
                                        NWidget(WWT_MATRIX),
                                        SetMinimalSize(1, 0),
                                        SetMatrixDataTip(1, 8, 7),
                                        SetScrollbar(WID_AIC_SCROLLBAR),
                                        NWidget(NWID_VSCROLLBAR),
                                        EndContainer(),
                                        EndContainer(),
                                        NWidget(NWID_SPACER),
                                        SetMinimalSize(0, 9),
                                        NWidget(WWT_FRAME),
                                        SetDataTip(5, 0),
                                        SetPadding(),
                                        NWidget(WWT_MATRIX),
                                        SetMinimalSize(1, 0),
                                        SetMatrixDataTip(1, 1, 6),
                                        EndContainer(),
                                        NWidget(NWID_HORIZONTAL),
                                        SetPadding(),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetFill(1, 0),
                                        SetMinimalSize(3, 2),
                                        SetDataTip(1, 5),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetFill(1, 0),
                                        SetMinimalSize(3, 2),
                                        SetDataTip(6, 7),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetFill(1, 0),
                                        SetMinimalSize(3, 2),
                                        SetDataTip(3, 0),
                                        EndContainer(),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetFill(1, 0),
                                        SetResize(1, 0),
                                        SetDataTip(6, 0),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetFill(1, 0),
                                        SetResize(1, 0),
                                        SetDataTip(7, 0),
                                        NWidget(WWT_PUSHTXTBTN),
                                        SetFill(1, 0),
                                        SetResize(1, 0),
                                        SetDataTip(2, 3)};
