typedef unsigned int PRUint32;
typedef int PRInt32;
typedef unsigned long PRUint64;
typedef int PRIntn;
typedef PRIntn PRBool;
struct nsRect {
    nsRect(const nsRect& aRect) { }
};
enum nsCompatibility { eCompatibility_NavQuirks = 3 };
class gfxContext;
typedef PRUint64 nsFrameState;
class nsPresContext {
public:
    nsCompatibility CompatibilityMode() const { }
};
class nsStyleContext {
public:
    PRBool HasTextDecorations() const;
};
class nsIFrame {
public:
    nsPresContext* PresContext() const;
    nsStyleContext* GetStyleContext() const;
    nsFrameState GetStateBits() const;
    nsRect GetOverflowRect() const;
};
class nsFrame : public nsIFrame { };
class nsLineList_iterator { };
class nsLineList {
public:
    typedef nsLineList_iterator iterator;
};
class gfxSkipCharsIterator { };
class gfxTextRun {
public:
    class PropertyProvider { };
};
class nsTextFrame : public nsFrame
{
  virtual nsRect ComputeTightBounds(gfxContext* aContext) const;
  gfxSkipCharsIterator EnsureTextRun(gfxContext* aReferenceContext = 0L,
				     nsIFrame* aLineContainer = 0L,
				     const nsLineList::iterator* aLine = 0L,
				     PRUint32* aFlowEndInTextRun = 0L);
};
class PropertyProvider : public gfxTextRun::PropertyProvider
{
public:
    PropertyProvider(nsTextFrame* aFrame, const gfxSkipCharsIterator& aStart);
    PRInt32 mLength[64];
};
nsRect nsTextFrame::ComputeTightBounds(gfxContext* aContext) const
{
  if ((GetStyleContext()->HasTextDecorations()
       && eCompatibility_NavQuirks == PresContext()->CompatibilityMode())
      || (GetStateBits() & (nsFrameState(1) << (23))))
    return GetOverflowRect();
  gfxSkipCharsIterator iter = const_cast<nsTextFrame*>(this)->EnsureTextRun();
  PropertyProvider provider(const_cast<nsTextFrame*>(this), iter);
}
