/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-inline" } */

#include "pr64049.h"

EnumStatusCode
ValueHelper::getLocalizedText (const ValueStruct* pValueStruct, LocalizedText& target)
{
	if (pValueStruct && pValueStruct->dataType == 0 && pValueStruct->arrayType == 0)
	{
		_ASSERTION (pValueStruct->value.LocalizedText, "Unexpected null pointer");
		return LocalizedTextSet (target.getInternHandle (), pValueStruct->value.LocalizedText);
	}
	else
	{
		return StatusCode::ERROR;
	}
}

LocalizedText
ValueHelper::getLocalizedText (const ValueStruct* pValueStruct)
{
	LocalizedText returnValue;
	EnumStatusCode status = getLocalizedText (pValueStruct, returnValue);
	_ASSERTION (StatusCode::isSUCCEEDED (status), "Conversion failed");
	return returnValue;
}

EnumStatusCode
LocalizedTextSet (LocalizedTextStruct* pTarget, LocalizedTextStruct* pSource)
{
	__builtin_strcpy (pTarget->getT (), pSource->getT ());
	return StatusCode::SUCCESS;
}

/* { dg-final { scan-ipa-dump-not "__builtin_unreachable" "inline" } } */
