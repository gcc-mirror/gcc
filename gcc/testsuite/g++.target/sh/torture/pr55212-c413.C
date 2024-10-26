/* { dg-additional-options "-std=c++20 -mlra -fpic -w " }  */
/* { dg-do compile }  */

struct Trans_NS_WTF_HashMap
{
  template <typename V> void set(int *, V);
};

struct AscentAndDescent
{
  float ascent;
  float descent;
};

bool isRubyAnnotationBox();

struct EnclosingAscentDescent
{
  float ascent;
  float descent;
};

AscentAndDescent primaryFontMetricsForInlineBox();
int adjustInlineBoxHeightsForLineBoxContainIfApplicable___trans_tmp_1;

void adjustInlineBoxHeightsForLineBoxContainIfApplicable()
{
  Trans_NS_WTF_HashMap inlineBoxBoundsMap;
  auto ensureFontMetricsBasedHeight = [&](auto inlineBox)
  {
    auto [ascent, descent] = primaryFontMetricsForInlineBox();
    auto halfLeading = isRubyAnnotationBox() ? ascent + descent : 0.f;
    ascent += halfLeading;
    inlineBoxBoundsMap.set(&inlineBox, EnclosingAscentDescent{ascent, descent});
  };
  ensureFontMetricsBasedHeight(
      adjustInlineBoxHeightsForLineBoxContainIfApplicable___trans_tmp_1);
}
